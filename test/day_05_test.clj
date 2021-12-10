(ns day_05-test
  (:require [clojure.test :refer [deftest is]]
            [day_05 :refer [part-01 part-02]]))

(def test-data
  '(((0 9) (5 9))
  ((8 0) (0 8))
  ((9 4) (3 4))
  ((2 2) (2 1))
  ((7 0) (7 4))
  ((6 4) (2 0))
  ((0 9) (2 9))
  ((3 4) (1 4))
  ((0 0) (8 8))
  ((5 5) (8 2))))

(deftest part-01-test
  (let [expected 5]
    (is (= expected (part-01 test-data)))))

(deftest part-02-test
  (let [expected 12]
    (is (= expected (part-02 test-data)))))
