(ns day_09-test
  (:require [clojure.test :refer [deftest is]]
            [day_09 :refer [part-01 part-02]]))

(def test-data ["2199943210"
                "3987894921"
                "9856789892"
                "8767896789"
                "9899965678"])

(deftest part-01-test
  (let [expected 15]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 1134]
    (is (= (part-02 test-data) expected))))
