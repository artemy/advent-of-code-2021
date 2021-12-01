(ns day_01_02
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

(def input-file "day-1-1.txt")

(def data-file (io/resource input-file))
(def data (->> data-file slurp split-lines (map #(Integer/parseInt %))))

(let [threes #(partition 3 1 %)
      pairs #(partition 2 1 %)
      sums #(reduce + %)
      differences (fn [pair] (- (first pair) (second pair)))]
  (->> data threes (map sums) pairs (map differences) (filter neg?) count))
