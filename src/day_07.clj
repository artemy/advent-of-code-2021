(ns day_07
  (:require [utils :refer [abs open-resource parse-int split-by-comma]]))

(def input-file "day_07.txt")

(def data (->> input-file open-resource first split-by-comma (map parse-int)))

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
  (let [calculate-fuel-cost (fn [x] (/ (* x (+ x 1)) 2))
        delta-calculator (fn [x d] (->> (- x d) abs calculate-fuel-cost))]
    (find-min-fuel-consumption delta-calculator input)))

(defn -main [& _]
  (println "Day 07:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
