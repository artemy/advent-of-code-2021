(ns day_13
  (:require [clojure.string :as str]
            [utils :refer [open-resource parse-int split-by-comma]]))

(def input-file "day_13.txt")

(def data (->> input-file
               open-resource))

(defn visualizer [input]
  (let [max-x (fn [v] (->> v (map first) (apply max) inc))
        max-y (fn [v] (->> v (map second) (apply max) inc))
        fill-grid (fn [v] (for [y (range 0 (max-y v))
                                x (range 0 (max-x v))]
                            (if (some #{(vector x y)} v)
                              "#"
                              ".")))]
    (->> input
         fill-grid
         (partition (max-x input))
         (map #(apply str %))
         (str/join "\n"))))

(defn prepare-data [input]
  (let [[raw-dots raw-instructions] (split-with not-empty input)
        create-coords #(->> % split-by-comma (map parse-int))
        dots (->> raw-dots (map create-coords) set)
        convert-types (fn [[direction position]]
                        (vector direction (parse-int position)))
        extract-instruction (fn [x] (->> x
                                         (re-find #"(\w)=(\d+)")
                                         (drop 1)
                                         convert-types))
        instructions (->> raw-instructions
                          (remove empty?)
                          (map extract-instruction))]
    {:dots         dots
     :instructions instructions}))

(defn perform-fold [input direction position]
  (let [on-the-fold? (fn [direction position]
                       #(= position
                           (case direction
                             "x" (first %)
                             "y" (second %))))
        new-coords (fn [c position] (- c (* 2 (- c position))))
        fold-x (fn [position]
                 #(let [[x y] %]
                    (if (> x position)
                      (vector (new-coords x position) y)
                      %)))
        fold-y (fn [position]
                 #(let [[x y] %]
                    (if (> y position)
                      (vector x (new-coords y position))
                      %)))
        combine-folded (fn [direction position]
                         (case direction
                           "x" (fold-x position)
                           "y" (fold-y position)))]
    (->> input
         (remove (on-the-fold? direction position))
         (map (combine-folded direction position))
         distinct)))

(defn perform-folds [dots instructions]
  (let [apply-fold (fn [d [direction position]] (perform-fold d direction position))]
    (reduce apply-fold dots instructions)))

(defn part-01 [input]
  (let [prepared-data (prepare-data input)
        dots (:dots prepared-data)
        instructions (->> prepared-data :instructions (take 1))]
    (->> (perform-folds dots instructions) count)))

(defn part-02 [input]
  (let [prepared-data (prepare-data input)
        dots (:dots prepared-data)
        instructions (:instructions prepared-data)]
    (->> (perform-folds dots instructions) visualizer (str "\n"))))

(defn -main [& _]
  (println "Day 13:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
