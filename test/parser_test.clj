(ns parser-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [parser :as p]))

((deftest number-test
   (testing "JSON numbers"
     (is (= (p/jsnumber "3.1415") [3.1415 nil]))
     (is (= (p/jsnumber "3.1415E+1") [31.415 nil])))))


((deftest escape
   (testing "JSON escape"
     (is (= (p/escape "\"") [\" nil]))
     (is (= (p/escape "\\") [\\ nil]))
     (is (= (p/escape "/") [\/ nil]))
     (is (= (p/escape "b") [\backspace nil]))
     (is (= (p/escape "f") [\formfeed nil]))
     (is (= (p/escape "n") [\newline nil]))
     (is (= (p/escape "r") [\return nil]))
     (is (= (p/escape "t") [\tab nil]))
     (is (= (p/escape "u0025") [\u0025 nil])))))


((deftest character
   (testing "JSON char"
     (is (= (p/character "a") [\a nil]))
     (is (= (p/character "\\\"") [\" nil]))
     (is (= (p/character "\\\\") [\\ nil]))
     (is (= (p/character "\\u0025") [\% nil]))
     (is (= (p/character "\\u00A9") [\© nil])))))

((deftest string
   (testing "JSON strings"
     (is (= (p/jsstring "\"abc\"") ["abc" nil]))
     (is (= (p/jsstring "\"apa\\\"korv\"") ["apa\"korv" nil]))
     (is (= (p/jsstring "\"apa\\\\korv\"") ["apa\\korv" nil])))))

((deftest array
   (testing "JSON array"
     (is (= (p/jsarray "[]") [[] nil]))
     (is (= (p/jsarray "[[true]]") [[[true]] nil]))
     (is (= (p/jsarray "[\"abc\"]") [["abc"] nil]))
     (is (= (p/jsarray "[\"abc\",3.14]") [["abc" 3.14] nil])))))

((deftest parsers
   (testing "literal"
     (is (= ((p/literal "abc") "abcdef") ["abc" '(\d \e \f)]))
     (is (= ((p/literal "abc") "abxxxxx") nil)))
   (testing "true"
     (is (= (p/jstrue "true") [true nil])))
   (testing "false"
     (is (= (p/jsfalse "false") [false nil])))
   (testing "null"
     (is (= (p/jsnull "null") [nil nil])))
   (testing "element"
     (is (= (p/element "      false      ") [false nil])))
   (testing "elements"
     (is (= (p/elements "\"apa\", null, true, false, 3.14") [["apa" nil true false 3.14] nil])))))
