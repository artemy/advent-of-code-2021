(ns day_19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.matrix :as matrix]
            [clojure.math.combinatorics :as combo]
            [utils :refer [parse-int split-by-comma]]))

(def input-file "day_19.txt")

(def data (-> input-file
              io/resource
              slurp))

(defn prepare-data
  [input]
  (let [split-by-scanner (fn [s] (str/split s #"\n*--- scanner \d* ---\n*"))
        split-coordinates (fn [s] (->> s (map split-by-comma) (map #(map parse-int %)) (map matrix/array)))]
    (->> input
         split-by-scanner
         (remove empty?)
         (map str/split-lines)
         (map split-coordinates)
         (map-indexed vector)
         (into {}))))

(def rotations
  [[[1 0 0] [0 1 0] [0 0 1]]
   [[0 -1 0] [1 0 0] [0 0 1]]
   [[-1 0 0] [0 -1 0] [0 0 1]]
   [[0 1 0] [-1 0 0] [0 0 1]]

   [[1 0 0] [0 0 -1] [0 1 0]]
   [[0 0 1] [1 0 0] [0 1 0]]
   [[-1 0 0] [0 0 -1] [0 -1 0]]
   [[0 0 1] [-1 0 0] [0 -1 0]]

   [[1 0 0] [0 -1 0] [0 0 -1]]
   [[0 1 0] [1 0 0] [0 0 -1]]
   [[-1 0 0] [0 1 0] [0 0 -1]]
   [[0 -1 0] [-1 0 0] [0 0 -1]]

   [[1 0 0] [0 0 1] [0 -1 0]]
   [[0 0 -1] [1 0 0] [0 -1 0]]
   [[-1 0 0] [0 0 1] [0 1 0]]
   [[0 0 -1] [-1 0 0] [0 1 0]]

   [[0 0 -1] [0 1 0] [1 0 0]]
   [[0 0 -1] [0 -1 0] [-1 0 0]]

   [[0 1 0] [0 0 1] [1 0 0]]
   [[0 -1 0] [0 0 1] [-1 0 0]]

   [[0 0 1] [0 -1 0] [1 0 0]]
   [[0 0 1] [0 1 0] [-1 0 0]]

   [[0 -1 0] [0 0 -1] [1 0 0]]
   [[0 1 0] [0 0 -1] [-1 0 0]]])

(defn distance-vector [beacon1 beacon2] (map - beacon1 beacon2))
(defn rotate-coords [rotation coords] (map int (matrix/mmul rotation coords)))

(defn mix [input] (combo/combinations input 2))

(defn match-scanners [input]
  (let [rotate-all-beacons (fn [[scanner-id beacons] rotation]
                             {:scanner-id scanner-id
                              :rotation   rotation
                              :beacons    (map #(rotate-coords rotation %) beacons)})
        coinciding-points (fn [beacon1 beacon2]
                            (->> (for [p1 beacon1
                                       p2 beacon2
                                       :when (= p1 p2)]
                                   [p1 p2])))
        compare-rotation (fn [[scanner1-id scanner1-beacons] {:keys [scanner-id rotation beacons]}]
                           (let [try-to-match (fn [scanner2-location]
                                                (let [scanner2-translated (map #(map + scanner2-location %) beacons)
                                                      matching-points (coinciding-points scanner1-beacons scanner2-translated)]
                                                  (when (->> matching-points count (<= 12))
                                                    {:scanner1-id       scanner1-id
                                                     :scanner2-id       scanner-id
                                                     :scanner2-location scanner2-location
                                                     :rotation          rotation})))]
                             (->> (for [p1 scanner1-beacons
                                        p2 beacons]
                                    (distance-vector p1 p2))
                                  (map try-to-match)
                                  (drop-while nil?)
                                  first)))
        match-scanners (fn [[scanner1 scanner2]]
                         (let [scanner2-rotations (map #(rotate-all-beacons scanner2 %) rotations)]
                           (->> scanner2-rotations
                                (map #(compare-rotation scanner1 %))
                                (drop-while nil?)
                                first)))
        reversed (fn [p {:keys [scanner1-id scanner2-id scanner2-location rotation] :as n}]
                   (conj p n
                         {:scanner1-id       scanner2-id
                          :scanner2-id       scanner1-id
                          :rotation          rotation
                          :scanner2-location (map int (matrix/sub [0 0 0] (matrix/mmul rotation scanner2-location)))}))]
    (->> input
         mix
         (map match-scanners)
         (remove nil?)
         (reduce reversed [])
         distinct)))

(defn find-absolute-scanner-coordinates-and-rotations [input]
  (let [find-absolute-coordinates (fn [p {:keys [scanner1-id scanner2-id scanner2-location rotation]}]
                                    (let [scanner1-absolute-location (get-in p [scanner1-id :location])
                                          scanner1-rotation (get-in p [scanner1-id :rotation])
                                          scanner2-absolute-location (map + scanner1-absolute-location (rotate-coords scanner1-rotation scanner2-location))
                                          scanner2-absolute-rotation (map #(map int %) (matrix/mmul scanner1-rotation rotation))]
                                      (assoc p scanner2-id
                                               {:location scanner2-absolute-location
                                                :rotation scanner2-absolute-rotation
                                                })))
        absolute-coordinates {0 {:location [0 0 0]
                                 :rotation [[1 0 0] [0 1 0] [0 0 1]]}}]
    (loop [absolute-coordinates absolute-coordinates]
      (let [can-be-located (fn [{:keys [scanner1-id scanner2-id]}]
                             (and (contains? absolute-coordinates scanner1-id) (not (contains? absolute-coordinates scanner2-id))))
            adjacent-scanners (filter can-be-located input)]
        (if (empty? adjacent-scanners)
          absolute-coordinates
          (recur (reduce find-absolute-coordinates absolute-coordinates adjacent-scanners)))))))

(defn find-absolute-beacon-coordinates [scanners absolute-scanner-coords]
  (let [translate-all-coords (fn [p [scanner-id beacons]]
                               (let [scanner-location (get-in absolute-scanner-coords [scanner-id :location])
                                     scanner-rotation (get-in absolute-scanner-coords [scanner-id :rotation])
                                     translate-coords (fn [coords] (map + scanner-location (rotate-coords scanner-rotation coords)))]
                                 (apply conj p (map translate-coords beacons))))]
    (->> scanners (reduce translate-all-coords []) distinct)))

(defn part-01 [input]
  (let [scanners (prepare-data input)
        absolute-scanner-coords (->> scanners match-scanners find-absolute-scanner-coordinates-and-rotations)]
    (->> absolute-scanner-coords
         (find-absolute-beacon-coordinates scanners)
         count)))

(defn part-02 [input]
  (let [scanners (prepare-data input)
        absolute-scanner-coords (->> scanners match-scanners find-absolute-scanner-coordinates-and-rotations)
        manhattan-distance (fn [[f s]] (reduce + (map #(Math/abs ^int %) (map - f s))))]
    (->> absolute-scanner-coords
         vals
         (map :location)
         mix
         (map manhattan-distance)
         (apply max))))

(defn -main [& _]
  (println "Day 19:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
