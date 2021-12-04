(ns day_04
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_04.txt")

(def data (let [split-by-comma #(str/split % #",")
                lines (->> input-file open-resource)]
            {:numbers (->> lines
                           first
                           split-by-comma
                           (map parse-int))
             :boards  (->> lines
                           rest
                           (remove str/blank?)
                           (map #(->> %
                                      str/trim
                                      split-whitespace
                                      (map parse-int)))
                           (partition 5))}))


(defn part-01 [input]
  (loop [boards (:boards input) numbers (:numbers input)]
    (let [current-number (first numbers)
          remaining-numbers (rest numbers)
          replace-number (fn [v x] (map (fn [y] (map #(if (= % x) "x" %) y)) v))
          current-boards (map #(replace-number % current-number) boards)
          all-marked? (fn [v] (every? #(= "x" %) v))
          winning-board? (fn [b] (let [transposed (transpose b)]
                                   (or
                                     (some #(all-marked? %) b)
                                     (some #(all-marked? %) transposed))))
          empty-boards (filter winning-board? current-boards)]
      (if (empty? empty-boards)
        (recur current-boards remaining-numbers)
        (* (->> empty-boards flatten (remove #(= "x" %)) (reduce +)) current-number)))))

(defn part-02 [input]
  (loop [boards (:boards input) numbers (:numbers input)]
    (let [current-number (first numbers)
          remaining-numbers (rest numbers)
          replace-number (fn [v x] (map (fn [y] (map #(if (= % x) "x" %) y)) v))
          current-boards (map #(replace-number % current-number) boards)
          all-marked? (fn [v] (every? #(= "x" %) v))
          winning-board? (fn [b] (let [transposed (transpose b)]
                                   (or
                                     (some #(all-marked? %) b)
                                     (some #(all-marked? %) transposed))))]
      (if (and (= (count current-boards) 1))
        (* (->> current-boards flatten (remove #(= "x" %)) (reduce +)) current-number)
        (recur (remove winning-board? current-boards) remaining-numbers)))))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
