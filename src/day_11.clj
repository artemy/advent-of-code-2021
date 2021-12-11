(ns day_11
  (:require [clojure.string :as str]
            [utils :refer [open-resource]]))

(def input-file "day_11.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [width (->> input first count)
        build-coordinates (fn [i x] [[(rem i width) (quot i width)] (Character/digit ^char x 10)])
        coordinate-map (fn [in] (->> in str/join (map-indexed build-coordinates) (into {})))]
    (->> input coordinate-map)))

(def adjacent-coord-modifiers [[-1 -1] [0 -1] [1 -1]
                               [-1 0] [1 0]
                               [-1 1] [0 1] [1 1]])

(defn find-nearest-points
  [coordinate-map]
  #(select-keys coordinate-map (map (fn [x] (map + x %)) adjacent-coord-modifiers)))

(defn increase-levels [v] (->> v (map #(vector (first %) (-> % second inc))) (into {})))

(defn single-step [v]
  (loop [levels v
         flashed {}]
    (let [nines-filter (fn [v] (filter #(< 9 (second %)) v))
          nines (->> levels nines-filter (remove #(contains? flashed (first %))) (into {}))
          reset-to-zero (fn [v] (->> v (map (fn [[k v]] (if (< 9 v) [k 0] [k v]))) (into {})))]
      (if (empty? nines)
        [(reset-to-zero levels) (count flashed)]
        (let [adjacent (->> nines (map #(->> % first ((find-nearest-points levels)) (map first))) (mapcat identity))
              update-by-flash (reduce (fn [p n] (if (contains? p n) (update p n inc) p)) levels adjacent)]
          (recur update-by-flash (merge flashed nines))))
      )))

(defn count-flashes [steps input]
  (let [loop-steps (fn [v n] (loop [levels v
                                    step 0
                                    flashes 0]
                               (if (>= step n)
                                 [levels flashes]
                                 (let [next-step-levels (single-step (increase-levels levels))]
                                   (recur (first next-step-levels) (inc step) (+ flashes (second next-step-levels)))))))]
    (-> input (loop-steps steps) second)))

(defn find-first-synchronized [input]
  (let [
        steps (fn [v] (loop [levels v
                             step 0]
                        (if (every? (fn [[_ v]] (= v 0)) levels)
                          [levels step]
                          (let [next-step-levels (single-step (increase-levels levels))]
                            (recur (first next-step-levels) (inc step))))))]
    (-> input steps second)))

(defn part-01 [input steps]
  (->> input prepare-data (count-flashes steps)))

(defn part-02 [input]
  (->> input prepare-data find-first-synchronized))

(defn -main [& _]
  (println "Day 09:")
  (println "\t-> Part 1: " (part-01 data 100))
  (println "\t-> Part 2: " (part-02 data)))
