(ns day_01
  (:require [utils :refer [open-resource]]))

(def input-file "day_01.txt")

(def data (->> input-file open-resource (map #(Integer/parseInt %))))

(defn part-01 [input]
  (let [pairs #(partition 2 1 %)
        differences (fn [[f s]] (- f s))]
    (->> input pairs (map differences) (filter neg?) count)))

(defn part-02 [input]
  (let [threes #(partition 3 1 %)
        pairs #(partition 2 1 %)
        sums #(reduce + %)
        differences (fn [pair] (- (first pair) (second pair)))]
    (->> input threes (map sums) pairs (map differences) (filter neg?) count)))

(defn -main [& _]
  (println "Day 01:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
