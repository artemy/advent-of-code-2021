(ns day_21-test
  (:require [clojure.test :refer [deftest is]]
            [day_21 :refer [part-01 part-02]]))

(def test-data ["Player 1 starting position: 4"
                "Player 2 starting position: 8"])

(deftest part-01-test
  (let [expected 739785]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 444356092776315]
    (is (= (part-02 test-data) expected))))
