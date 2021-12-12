(ns day_12-test
  (:require [clojure.test :refer [deftest is]]
            [day_12 :refer [part-01 part-02]]))

(def test-data ["start-A"
                "start-b"
                "A-c"
                "A-b"
                "b-d"
                "A-end"
                "b-end"])

(def test-data-large ["fs-end"
                      "he-DX"
                      "fs-he"
                      "start-DX"
                      "pj-DX"
                      "end-zg"
                      "zg-sl"
                      "zg-pj"
                      "pj-he"
                      "RW-he"
                      "fs-DX"
                      "pj-RW"
                      "zg-RW"
                      "start-pj"
                      "he-WI"
                      "zg-he"
                      "pj-fs"
                      "start-RW"])

(deftest part-01-test
  (let [expected 10]
    (is (= (part-01 test-data) expected))))

(deftest part-01-large-data-test
  (let [expected 226]
    (is (= (part-01 test-data-large) expected))))

(deftest part-02-test
  (let [expected 36]
    (is (= (part-02 test-data) expected))))

(deftest part-02-large-data-test
  (let [expected 3509]
    (is (= (part-02 test-data-large) expected))))
