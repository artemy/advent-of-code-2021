(ns day_15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]
            [utils :refer [open-resource]]))

(def input-file "day_15.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [width (->> input first count)
        build-coordinates (fn [i x] [[(rem i width) (quot i width)] (Character/digit ^char x 10)])
        coordinate-map (fn [in] (->> in str/join (map-indexed build-coordinates) (into {})))]
    {:coordinates (->> input coordinate-map)
     :width       width}))


(def adjacent-coord-modifiers [[0 -1] [-1 0] [1 0] [0 1]])

; thanks to Michael Ummels for dijkstra implementation https://www.ummels.de/2014/06/08/dijkstra-in-clojure/

(defn successors
  ([coordinates] #(select-keys coordinates (map (fn [c] (map + c %)) adjacent-coord-modifiers))))

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.

  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn extend-map [input times]
  (let [{:keys [coordinates width]} input
        new-coordinates (fn [c direction] (->> c (iterate (fn [[k v]] [(mapv + k direction) (-> v inc (rem 10) (max 1))])) (take times) (into {})))
        extend-right (fn [d c] (merge d (new-coordinates c [width 0])))
        extend-down (fn [d c] (merge d (new-coordinates c [0 width])))]
    {:coordinates (-> coordinates (#(reduce extend-right % %)) (#(reduce extend-down % %)))
     :width       (* width times)}))

(defn part-01 [input]
  (let [{:keys [coordinates width]} (-> input prepare-data)
        target (->> width dec (repeat 2))]
    (get (dijkstra [0 0] (successors coordinates)) target)))

(defn part-02 [input]
  (let [{:keys [coordinates width]} (-> input prepare-data (extend-map 5))
        target (->> width dec (repeat 2))]
    (get (dijkstra [0 0] (successors coordinates)) target)))

(defn -main [& _]
  (println "Day 15:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
