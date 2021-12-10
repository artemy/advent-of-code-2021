(ns day_02-test
  (:require [clojure.test :refer [deftest is]]
            [day_02 :refer [part-02]]))

(deftest part-02-test
  (let [test-data [["forward" 5]
                   ["down" 5]
                   ["forward" 8]
                   ["up" 3]
                   ["down" 8]
                   ["forward" 2]]
        expected 900]
    (is (= expected (part-02 test-data)))))
