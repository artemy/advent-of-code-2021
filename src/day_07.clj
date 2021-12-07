(ns day_07
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_07.txt")

(def data (let [split-by-comma #(str/split % #",")]
            (->> input-file open-resource first split-by-comma (map parse-int))))

(defn calculate-fuel-consumptions [f v]
  (loop [delta (- (apply max v) (apply min v))
         fuel []]
    (let [find-delta (fn [x] (f x delta))]
      (if (> delta 0)
        (recur (dec delta) (->> v (map find-delta) (reduce +) (conj fuel)))
        fuel))))

(defn find-min-fuel-consumption [f v]
  (->> v (calculate-fuel-consumptions f) (apply min)))

(defn part-01 [input]
  (let [delta-calculator (fn [x d] (abs (- x d)))]
    (find-min-fuel-consumption delta-calculator input)))

(defn part-02 [input]
  (let [delta-calculator (fn [x d] (->> (- x d) abs inc (range 1) (reduce +)))]
    (find-min-fuel-consumption delta-calculator input)))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
