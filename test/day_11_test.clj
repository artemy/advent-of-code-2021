(ns day_11-test
  (:require [clojure.test :refer [deftest is]]
            [day_11 :refer [part-01 part-02]]))

(def test-data ["5483143223"
                "2745854711"
                "5264556173"
                "6141336146"
                "6357385478"
                "4167524645"
                "2176841721"
                "6882881134"
                "4846848554"
                "5283751526"])

(deftest part-01-test
  (let [expected 1656]
    (is (= (part-01 test-data 100) expected))))

(deftest part-02-test
  (let [expected 195]
    (is (= (part-02 test-data) expected))))
