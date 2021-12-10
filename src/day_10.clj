(ns day_10
  (:require [utils :refer [open-resource find-middle]]))

(def input-file "day_10.txt")

(def data (->> input-file
               open-resource))

(def bracket-map {\) \(
                  \] \[
                  \} \{
                  \> \<
                  \( \)
                  \[ \]
                  \{ \}
                  \< \>})

(defn find-first-illegal-char
  "Scans string for first incorrectly closing parens
   and returns that character and a list of already opened parens"
  [input]
  (loop [chars (rest input)
         closing-stack (list (first input))]
    (let [current-char (first chars)
          opening? (some #{current-char} [\( \[ \{ \<])]
      (if opening?
        (recur (rest chars) (conj closing-stack current-char))
        (if (not= (peek closing-stack) (get bracket-map current-char))
          [current-char closing-stack]
          (recur (rest chars) (pop closing-stack)))))))

(def points-lookup-part-01 {\) 3
                            \] 57
                            \} 1197
                            \> 25137})

(defn part-01 [input]
  (->> input (map find-first-illegal-char) (map first) (remove nil?) (map #(get points-lookup-part-01 %)) (reduce +)))


(def points-lookup-part-02 {\( 1
                            \[ 2
                            \{ 3
                            \< 4})

(defn calculate-score [p n] (->> p (* 5) (+ n)))

(defn part-02 [input]
  (->> input
       (map find-first-illegal-char)
       (filter #(nil? (first %)))
       (map second)
       (map #(map (fn [c] (get points-lookup-part-02 c)) %))
       (map #(reduce calculate-score 0 %))
       sort
       find-middle))

(defn -main [& _]
  (println "Day 10:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
