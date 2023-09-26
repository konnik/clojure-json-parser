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

((deftest member
   (testing "member"
     (is (= (p/member "\"a\": 3.14") [{"a" 3.14} nil])))))

((deftest members
   (testing "members"
     (is (= (p/members "\"a\": 3.14, \"b\" : true") [{"a" 3.14 "b" true} nil])))))

((deftest jsobject
   (testing "JSON object"
     (is (= (p/jsobject "{ \"a\": 3.14, \"b\" : true }") [{"a" 3.14 "b" true} nil])))))


((deftest parse-json
   (testing "Parse JSON - object"
     (is (= (p/parse-json "{ \"a\": 3.14, \"b\" : true }") {"a" 3.14 "b" true})))

   (testing "Parse JSON - array"
     (is (= (p/parse-json "[\"a string\",true,false,3.14,null,{\"apa\":42}]")
            ["a string",true,false,3.14,nil, {"apa" 42.0}])))

   (testing "Parse JSON - number"
     (is (= (p/parse-json "3.1415E+2")
            314.15)))

   (testing "Parse JSON - false"
     (is (= (p/parse-json "false")
            false)))

   (testing "Parse JSON - true"
     (is (= (p/parse-json "true")
            true)))

   (testing "Parse JSON - null"
     (is (= (p/parse-json "null")
            nil)))

   (testing "Parse JSON - string"
     (is (= (p/parse-json "\"a string\"") "a string")))

   (testing "Parse JSON - unparsed chars gives error "
     (is (= (p/parse-json "true korv") :parse_error_unparsed_chars)))

   (testing "Parse JSON - invalid json"
     (is (= (p/parse-json "lsajdlkasj") :parse_error_invalid_json)))))
