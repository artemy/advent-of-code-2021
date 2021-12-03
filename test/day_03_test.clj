(ns day_03-test
  (:require [clojure.test :refer [deftest is]]
            [utils :refer :all]
            [day_03 :refer [part-01 part-02]]))

(def test-data '((0 0 1 0 0)
                 (1 1 1 1 0)
                 (1 0 1 1 0)
                 (1 0 1 1 1)
                 (1 0 1 0 1)
                 (0 1 1 1 1)
                 (0 0 1 1 1)
                 (1 1 1 0 0)
                 (1 0 0 0 0)
                 (1 1 0 0 1)
                 (0 0 0 1 0)
                 (0 1 0 1 0)))

(deftest part-01-test
  (let [expected 198]
    (is (= expected (part-01 test-data)))))

(deftest part-02-test
  (let [expected 230]
    (is (= expected (part-02 test-data)))))
