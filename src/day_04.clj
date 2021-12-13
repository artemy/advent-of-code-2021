(ns day_04
  (:require [clojure.string :as str]
            [utils :refer [open-resource parse-int split-by-comma split-whitespace transpose]]))

(def input-file "day_04.txt")

(def data (let [lines (->> input-file open-resource)]
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

(defn all-marked? [v] (every? #(= "x" %) v))
(defn replace-number [v x] (map (fn [y] (map #(if (= % x) "x" %) y)) v))
(defn mark-number [v x] (map #(replace-number % x) v))
(defn winning-board? [b] (let [transposed (transpose b)]
                           (or
                             (some #(all-marked? %) b)
                             (some #(all-marked? %) transposed))))
(defn sum-remaining [b] (->> b flatten (remove #(= "x" %)) (reduce +)))

(defn part-01 [input]
  (loop [boards (:boards input) numbers (:numbers input)]
    (let [[current-number & remaining-numbers] numbers
          updated-boards (mark-number boards current-number)
          winning-boards (filter winning-board? updated-boards)]
      (if (empty? winning-boards)
        (recur updated-boards remaining-numbers)
        (* (sum-remaining winning-boards) current-number)))))



(defn part-02 [input]
  (loop [boards (:boards input) numbers (:numbers input)]
    (let [[current-number & remaining-numbers] numbers
          updated-boards (mark-number boards current-number)
          boards-yet-to-win (remove winning-board? updated-boards)]
      (if (empty? boards-yet-to-win)
        (* (sum-remaining updated-boards) current-number)
        (recur boards-yet-to-win remaining-numbers)))))

(defn -main [& _]
  (println "Day 04:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
