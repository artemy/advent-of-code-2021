(ns day_25-test
  (:require [clojure.test :refer [deftest is]]
            [day_25 :refer [part-01]]))

(def test-data ["v...>>.vv>"
                ".vv>>.vv.."
                ">>.>v>...v"
                ">>v>>.>.v."
                "v>v.vv.v.."
                ">.>>..v..."
                ".vv..>.>v."
                "v.v..>>v.v"
                "....v..v.>"])

(deftest part-01-test
  (let [expected 58]
    (is (= (part-01 test-data) expected))))
