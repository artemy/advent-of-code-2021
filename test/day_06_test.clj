 (ns day_06-test
  (:require [clojure.test :refer [deftest is]]
            [day_06 :refer [part-01]]))

(def test-data '(3 4 3 1 2))

(deftest part-01-test
  (let [expected 5934]
    (is (= expected (part-01 test-data 80)))))

(deftest part-02-test
  (let [expected 26984457539]
    (is (= expected (part-01 test-data 256)))))
