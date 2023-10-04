(ns parser)

(defn char-literal [char-to-match]
  (fn [input]
    (when-some [[x & xs] input]
      (when (= char-to-match x) [x xs]))))


(defn char-match [test]
  (fn [input]
    (when-some [[x & xs] input]
      (when (test x) [x xs]))))

(defn one-of [& parsers]
  (fn [input]
    (some #(%1 input) parsers)))

(defn fmap [f parser]
  (fn [xs] (let [[a as] (parser xs)]
             (when (not (nil? a)) [(f a) as]))))

(defn succeed [value]
  (fn [input]
    [value (seq input)]))

(defn blank [parser] (fmap (constantly "") parser))

(defn pconcat
  "Kombinera två parsers till en där restultatet blir 
   sträng-konkaterneringen av de båda resultaten."
  [parser-a parser-b]
  (fn [input]
    (when-some [[a as] (parser-a input)]
      (when-some [[b bs] (parser-b as)]
        [(str a b) bs]))))

(defn chain
  "Kombinera fler parsers till en där resultatet från 
   alla parsers kombineras till en sträng."
  [& parsers]
  (reduce pconcat parsers))

(defn and-then [parser-a a-to-parser-b]
  (fn [xs]
    (when-some [[a as] (parser-a xs)]
      ((a-to-parser-b a) as))))

(defn keep-second
  "Kombinera två parsers och kasta bort resultatet från den första."
  [parser-a parser-b]
  (fn [input]
    (when-some [[_ as] (parser-a input)]
      (parser-b as))))

(defn keep-first
  "Kombinera två parsers och kasta bort resultatet från den andra"
  [parser-a parser-b]
  (and-then parser-a (fn [a] (fmap (constantly a) parser-b))))

;; parsers

(def zero (char-literal \0))
(def minus (char-literal \-))
(def plus (char-literal \+))
(def dot (char-literal \.))
(def onenine (char-match #{\1 \2 \3 \4 \5 \6 \7 \8 \9}))
(def hex (char-match #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A \B \C \D \E \F \a \b \c \d \e \f}))

(def digit (one-of zero onenine))

(def digits
  (one-of
   (chain digit #(digits %))
   digit))

(def integer
  (one-of
   (chain minus onenine digits)
   (chain minus digit)
   (chain onenine digits)
   (chain digit)))

(defn ws [input]
  ((one-of
    (keep-second (char-literal \u0020) #(ws %1))
    (keep-second (char-literal \u000A) #(ws %1))
    (keep-second (char-literal \u000D) #(ws %1))
    (keep-second (char-literal \u0009) #(ws %1))
    (succeed "")) input))


(def fraction
  (one-of
   (chain dot digits)
   (succeed "")))


(def sign
  (one-of
   plus
   minus
   (succeed "")))

(def exponent
  (one-of
   (chain (char-literal \e) sign digits)
   (chain (char-literal \E) sign digits)
   (succeed "")))


(def jsnumber
  (fmap #(Double/parseDouble %1)
        (chain integer fraction exponent)))

(def hex-character
  (fmap (fn [x] (char (read-string (str "0x" x))))
        (chain
         (keep-second (char-literal \u) hex)
         hex
         hex
         hex)))

(def escape
  (one-of
   (char-literal \")
   (char-literal \\)
   (char-literal \/)
   (keep-second (char-literal \b) (succeed \backspace))
   (keep-second (char-literal \f) (succeed \formfeed))
   (keep-second (char-literal \n) (succeed \newline))
   (keep-second (char-literal \r) (succeed \return))
   (keep-second (char-literal \t) (succeed \tab))
   hex-character))


(def character
  (one-of
   (char-match (complement #{\" \\}))
   (keep-second (char-literal \\) escape)))

(def characters
  (one-of
   (chain character (fn [input] (characters input)))
   (succeed "")))

(def jsstring
  (chain (blank (char-literal \")) characters (blank (char-literal \"))))

(defn literal [literal-value]
  (fn [input]
    (let [literal-seq  (seq literal-value)
          literal-count (count literal-seq)
          input-prefix (take literal-count input)]
      (when (= literal-seq input-prefix)
        [literal-value (drop  literal-count input)]))))


(def jstrue (and-then (literal "true") (constantly (succeed true))))
(def jsfalse (and-then (literal "false") (constantly (succeed false))))
(def jsnull (and-then (literal "null") (constantly (succeed nil))))

(declare jsarray)
(declare jsobject)
(def jsvalue
  (one-of
   jsnull
   jsfalse
   jstrue
   jsnumber
   jsstring
   (fn [input] (jsarray input)) ;hur kan man forward referera till jsarray??
   (fn [input] (jsobject input)) ;hur kan man forward referera till jsobject
   ))

(def element (keep-second ws (keep-first jsvalue ws)))

(def elements
  (one-of
   (and-then element
             (fn [a-value] (keep-second
                            (char-literal \,)
                            (fmap #(cons a-value %1) elements))))
   (fmap vector element)))

(def jsarray
  (one-of
   (keep-second (char-literal \[) (keep-second ws (keep-second (char-literal \]) (succeed []))))
   (keep-second (char-literal \[) (keep-first elements (char-literal \])))))


(defn pconcat2
  "Kombinera två parsers till en där restultatet blir 
   vekotr-konkaterneringen av de båda resultaten."
  [parser-a parser-b]
  (fn [input]
    (when-some [[a as] (parser-a input)]
      (when-some [[b bs] (parser-b as)]
        [(conj a b) bs]))))

(defn chain2
  "Chains multiple parsers into one and return the results from
  each individual parser as a vector."
  [& parsers]
  (reduce pconcat2 (succeed []) parsers))


(def member
  (fmap
   #(hash-map (nth %1 1) (nth %1 4))
   (chain2
    ws
    jsstring
    ws
    (char-literal \:)
    element)))

(def members
  (one-of
   (fmap #(merge (nth %1 0) (nth %1 2))
         (chain2
          member
          (char-literal \,)
          (fn [input] (members input))))
   member))

(def jsobject
  (one-of
   (keep-second (char-literal \{) (keep-second ws (keep-second (char-literal \}) (succeed {}))))
   (keep-second (char-literal \{) (keep-first members (char-literal \})))))


(defn parse-json [input]
  (let [result (jsvalue input)
        [value unparsed-chars] result]
    (cond
      (nil? result) :parse_error_invalid_json
      (not (nil? unparsed-chars)) :parse_error_unparsed_chars
      :else value)))
