(ns day_11
  (:require [clojure.string :as str]
            [utils :refer [flatten-first open-resource]]))

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

(defn increase-all-levels [v] (->> v (map #(vector (first %) (-> % second inc))) (into {})))

(defn single-step [v]
  (loop [levels v
         flashed {}]
    (let [greater-than-nine? (fn [x] (->> x second (< 9)))
          has-flashed? (fn [x] (contains? flashed (first x)))
          flashing (->> levels (filter greater-than-nine?) (remove has-flashed?))
          reset-flashed (fn [[k v]] (if (< 9 v) [k 0] [k v]))]
      (if (empty? flashing)
        [(->> levels (map reset-flashed)) (count flashed)]
        (let [adjacent-to-flashed (->> flashing (map first) (map #((find-nearest-points levels) %)) flatten-first (map first))
              increase-if-adjacent (fn [v c] (if (contains? v c)
                                               (update v c inc)
                                               v))]
          (recur (reduce increase-if-adjacent levels adjacent-to-flashed) (merge flashed flashing))))
      )))

(defn count-flashes [steps input]
  (let [loop-steps (fn [v n] (loop [levels v
                                    step 0
                                    flashes 0]
                               (if (>= step n)
                                 [levels flashes]
                                 (let [next-step-levels (-> levels increase-all-levels single-step)]
                                   (recur (first next-step-levels)
                                          (inc step)
                                          (->> next-step-levels second (+ flashes)))))))]
    (-> input (loop-steps steps) second)))

(defn find-first-synchronized [input]
  (let [loop-steps (fn [v] (loop [levels v
                                  step 0]
                             (if (every? (fn [[_ v]] (= v 0)) levels)
                               [levels step]
                               (let [next-step-levels (-> levels increase-all-levels single-step)]
                                 (recur (first next-step-levels)
                                        (inc step))))))]
    (-> input loop-steps second)))

(defn part-01 [input steps]
  (->> input prepare-data (count-flashes steps)))

(defn part-02 [input]
  (->> input prepare-data find-first-synchronized))

(defn -main [& _]
  (println "Day 11:")
  (println "\t-> Part 1: " (part-01 data 100))
  (println "\t-> Part 2: " (part-02 data)))
