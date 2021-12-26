(ns day_25
  (:require [clojure.data.priority-map :refer [priority-map]]
            [utils :refer [open-resource parse-int transpose]]
            [clojure.string :as str]))

(def input-file "day_25.txt")

(def data (-> input-file
              open-resource))

(defn prepare-data [input]
  (let [width (->> input first count)
        build-coordinates (fn [i x] [[(rem i width) (quot i width)] x])
        coordinate-map (fn [in] (->> in str/join (map-indexed build-coordinates) (into {})))]
    (->> input
         coordinate-map
         (remove (fn [[_ v]] (= \. v)))
         (into {})
         (assoc {:width width :height (->> input count)} :state))))

(defn visualizer [{:keys [width height state]}]
  (let [fill-grid (for [y (range 0 height)
                        x (range 0 width)]
                    (get state [x y] \.))]
    (->> fill-grid
         (partition width)
         (map #(apply str %))
         (str/join "\n"))))

(defn process-input [input]
  (let [{:keys [width height]} input
        find-adjacent (fn [[[x y] c]]
                        (case c
                          \> [(-> x (+ 1) (mod width)) y]
                          \v [x (-> y (+ 1) (mod height))]))
        can-move? (fn [cell state] (->> cell find-adjacent (contains? state) not))
        move-single (fn [state [coords c :as cell]]
                      (-> state
                          (assoc (find-adjacent cell) c)
                          (dissoc coords)))
        east-facing? (fn [[_ c]] (= \> c))
        south-facing? (fn [[_ c]] (= \v c))
        available-moves-for-direction (fn [f state] (->> state (filter f) (filter #(can-move? % state))))
        move-all (fn [f state]
                   (let [available-moves (available-moves-for-direction f state)]
                     (if (empty? available-moves)
                       state
                       (reduce move-single state available-moves))))
        perform-moves (fn [in]
                        (loop [game in
                               moves 1]
                          (let [{:keys [state]} game]
                            (if (and
                                  (empty? (available-moves-for-direction east-facing? state))
                                  (empty? (available-moves-for-direction south-facing? state)))
                              [game moves]
                              (recur (assoc game :state (->> state
                                                             (move-all east-facing?)
                                                             (move-all south-facing?)))
                                     (inc moves))))))]
    (->> input perform-moves)))

(defn part-01 [input]
  (->> input
       prepare-data
       process-input
       second))

(defn -main [& _]
  (println "Day 22:")
  (println "\t-> Part 1: " (part-01 data)))
