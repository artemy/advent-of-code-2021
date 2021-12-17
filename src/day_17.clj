(ns day_17
  (:require [utils :refer [open-resource parse-int]]))

(def input-file "day_17.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [[_ & x] (re-find #"x\=(\d+)..(\d+)" input)
        [_ & y] (re-find #"y\=(-?\d+)..(-?\d+)" input)
        [x1 x2] (map parse-int x)
        [y1 y2] (map parse-int y)]
    {:x1 x1 :x2 x2 :y1 y1 :y2 y2}))

(defn max-y [y1] (/ (* y1 (+ y1 1)) 2))

(defn shoot [[x-velocity y-velocity] box]
  (loop [[x y] [0 0]
         [x-velocity y-velocity] [x-velocity y-velocity]
         positions []]
    (if (or
          (> x (:x2 box))                                   ; too far by X
          (< y (:y1 box))                                   ; too low by Y
          (and (< y (:y1 box)) (< x (:x1 box))))            ; before the box
      false
      (if (and
            (<= (:x1 box) x (:x2 box))
            (<= (:y1 box) y (:y2 box)))                     ; in the box
        positions
        (let [new-coords (map + [x y] [x-velocity y-velocity])
              change-x-velocity (fn [x-velocity]
                                  (cond
                                    (neg? x-velocity) (inc x-velocity)
                                    (pos? x-velocity) (dec x-velocity)
                                    (zero? x-velocity) (identity x-velocity)))]
          (recur new-coords
                 [(change-x-velocity x-velocity) (dec y-velocity)]
                 (conj positions new-coords))))
      )))

(defn part-01 [input]
  (->> input first prepare-data :y1 max-y))

(defn part-02 [input]
  (let [prepared-data (->> input first prepare-data)
        [x-min x-max] [(Math/floor (Math/sqrt (* (:x1 prepared-data) 2))) (inc (:x2 prepared-data))]
        [y-min y-max] [(:y1 prepared-data) (inc (Math/abs ^int (:y1 prepared-data)))]
        coords (for [x (range x-min x-max) y (range y-min y-max)] [x y])]
    (->> coords (map #(shoot % prepared-data)) (filter identity) count)))

(defn -main [& _]
  (println "Day 17:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
