 (ns day_07-test
  (:require [clojure.test :refer [deftest is]]
            [day_07 :refer [part-01 part-02]]))

(def test-data '(16 1 2 0 4 2 7 1 2 14))

(deftest part-01-test
  (let [expected 37]
    (is (= expected (part-01 test-data)))))

(deftest part-02-test
  (let [expected 168]
    (is (= expected (part-02 test-data)))))
