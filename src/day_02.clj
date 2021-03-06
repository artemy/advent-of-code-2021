(ns day_02
  (:require [utils :refer [open-resource parse-int split-whitespace]]))

(def input-file "day_02.txt")

(def data (->> input-file open-resource (map split-whitespace) (map #(assoc % 1 (parse-int (second %))))))

(defn part-01 [input]
  (let [values-reducer (fn [[horizontal depth] [command value]]
                         (case command
                           "forward" (vector (+ horizontal value) depth)
                           "down" (vector horizontal (+ depth value))
                           "up" (vector horizontal (- depth value))))]
    (->> input (reduce values-reducer [0 0]) (reduce *))))

(defn part-02 [input]
  (let [forward (fn [[horizontal depth aim] value] (vector (+ horizontal value) (+ depth (* aim value)) aim))
        down (fn [[horizontal depth aim] value] (vector horizontal depth (+ aim value)))
        up (fn [[horizontal depth aim] value] (vector horizontal depth (- aim value)))
        values-reducer (fn [prev [command value]]
                         (case command
                           "forward" (forward prev value)
                           "down" (down prev value)
                           "up" (up prev value)))]
    (->> input (reduce values-reducer [0 0 0]) (take 2) (reduce *))))

(defn -main [& _]
  (println "Day 02:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
