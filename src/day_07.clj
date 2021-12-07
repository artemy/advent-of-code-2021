(ns day_07
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_07.txt")

(def data (let [split-by-comma #(str/split % #",")]
            (->> input-file open-resource first split-by-comma (map parse-int))))

(defn find-fuel-consumptions [f v]
  (loop [delta (- (apply max v) (apply min v))
         fuel []]
    (if (> delta 0)
      (recur (dec delta) (conj fuel (reduce + (map #(f % delta) v))))
      fuel)))

(defn part-01 [input]
  (let [get-delta (fn [x d] (abs (- x d)))]
    (->> input (find-fuel-consumptions get-delta) (apply min))))

(defn part-02 [input]
  (let [get-delta (fn [x d] (reduce + (range 1 (inc (abs (- x d))))))
        ]
    (->> input (find-fuel-consumptions get-delta) (apply min))))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
