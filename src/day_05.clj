(ns day_05
  (:require [clojure.string :as str]
            [utils :refer [flatten-first open-resource parse-int split-by-comma]]))

(def input-file "day_05.txt")

(def data (let [split-by-arrow #(str/split % #"\s->\s")]
            (->> input-file
                 open-resource
                 (map split-by-arrow)
                 (map #(->> % (map split-by-comma) (map (fn [x] (map parse-int x))))))))

(defn inclusive-range [s e]
  (let [reversed? (< e s)]
    (range s
           ((if reversed?
              dec
              inc) e)
           (if reversed? -1 1))))

(defn create-lines [[[x1 y1] [x2 y2]]]
  (let [xs (inclusive-range x1 x2)
        ys (inclusive-range y1 y2)]
    (if (= (count xs) (count ys))
      (map vector xs ys)
      (for [x xs y ys] [x y]))))

(defn part-01 [input]
  (let [diagonal? (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2)))]
    (->> input
         (filter diagonal?)
         (map create-lines)
         flatten-first
         frequencies
         (filter #(-> % val (>= 2)))
         count)))

(defn part-02 [input]
  (->> input
       (map create-lines)
       flatten-first
       frequencies
       (filter #(-> % val (>= 2)))
       count))

(defn -main [& _]
  (println "Day 05:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
