(ns day_17-test
  (:require [clojure.test :refer [deftest is]]
            [day_17 :refer [part-01 part-02]]))

(def test-data ["target area: x=20..30, y=-10..-5"])

(deftest part-01-test
  (let [expected 45]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 112]
    (is (= (part-02 test-data) expected))))
