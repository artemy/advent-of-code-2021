(ns day_09
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_09.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [width (->> input first count)
        build-coordinates (fn [i x] [[(rem i width) (quot i width)] (Character/digit ^char x 10)])
        coordinate-map (fn [in] (->> in str/join (map-indexed build-coordinates) (into {})))]
    (->> input coordinate-map)))

(defn find-top [[x y]] [(dec x) y])
(defn find-bottom [[x y]] [(inc x) y])
(defn find-left [[x y]] [x (dec y)])
(defn find-right [[x y]] [x (inc y)])
(def adjacent-coord-fns [find-top find-bottom find-left find-right])

(defn find-nearest-points
  [coordinate-map]
  #(select-keys coordinate-map (map (fn [f] (f %)) adjacent-coord-fns)))

(defn find-lowest-points [input]
  (let [lowest? (fn [[coords v]] (< v (apply min (-> coords ((find-nearest-points input)) vals))))]
    (->> input (filter lowest?))))

(defn find-basins [input]
  (let [find-basin (fn [coords]
                     (loop [basin {}
                            edge-points [coords]]
                       (if (empty? edge-points)
                         basin
                         (let [nearest-points (->> edge-points keys (map (find-nearest-points input)) (into {}))
                               new-edge-points (remove (fn [[k v]] (or (contains? basin k) (= v 9))) nearest-points)]
                           (recur (conj basin new-edge-points) new-edge-points)))))]
    (->> input find-lowest-points (map find-basin))))

(defn part-01 [input]
  (->> input prepare-data find-lowest-points vals (map inc) (reduce +)))

(defn part-02 [input]
  (->> input prepare-data find-basins (map count) sort (take-last 3) (reduce *)))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
