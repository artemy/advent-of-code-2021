(ns day_13-test
  (:require [clojure.test :refer [deftest is]]
            [day_13 :refer [part-01 part-02]]))

(def test-data ["6,10"
                "0,14"
                "9,10"
                "0,3"
                "10,4"
                "4,11"
                "6,0"
                "6,12"
                "4,1"
                "0,13"
                "10,12"
                "3,4"
                "3,0"
                "8,4"
                "1,10"
                "2,14"
                "8,10"
                "9,0"
                ""
                "fold along y=7"
                "fold along x=5"])


(deftest part-01-test
  (let [expected 17]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected (str "\n"
                      "#####\n"
                      "#...#\n"
                      "#...#\n"
                      "#...#\n"
                      "#####")]
    (is (= (part-02 test-data) expected))))
