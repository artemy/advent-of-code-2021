(ns day_01_01
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

(def input-file "day-1-1.txt")

(def data-file (io/resource input-file))
(def data (->> data-file slurp split-lines (map #(Integer/parseInt %))))

(let [pairs #(partition 2 1 %)
      differences (fn [[f s]] (- f s))]
  (->> data pairs (map differences) (filter neg?) count))
