(ns day_15-test
  (:require [clojure.test :refer [deftest is]]
            [day_15 :refer [part-01 part-02]]))

(def test-data ["1163751742"
                "1381373672"
                "2136511328"
                "3694931569"
                "7463417111"
                "1319128137"
                "1359912421"
                "3125421639"
                "1293138521"
                "2311944581"])


(deftest part-01-test
  (let [expected 40]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 315]
    (is (= (part-02 test-data) expected))))
