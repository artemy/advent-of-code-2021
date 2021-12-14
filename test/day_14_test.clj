(ns day_14-test
  (:require [clojure.test :refer [deftest is]]
            [day_14 :refer [part-01 part-02]]))

(def test-data ["NNCB"
                ""
                "CH -> B"
                "HH -> N"
                "CB -> H"
                "NH -> C"
                "HB -> C"
                "HC -> B"
                "HN -> C"
                "NN -> C"
                "BH -> H"
                "NC -> B"
                "NB -> B"
                "BN -> B"
                "BB -> N"
                "BC -> B"
                "CC -> N"
                "CN -> C"])


(deftest part-01-test
  (let [expected 1588]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 2188189693529]
    (is (= (part-02 test-data) expected))))
