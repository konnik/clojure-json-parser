(ns test-runner
  (:require [clojure.test :refer [run-tests]]
            [parser-test]))

(defn -main []
  (run-tests 'parser-test)) 