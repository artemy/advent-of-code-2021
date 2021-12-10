(ns day_08
  (:require [clojure.string :as str]
            [utils :refer [open-resource parse-int split-whitespace]]))

(def input-file "day_08.txt")

(def data (let [split-by-pipe #(str/split % #"\|")]
            (->> input-file
                 open-resource
                 (map (comp #(map split-whitespace %)
                            #(map str/trim %)
                            split-by-pipe)))))

; from reddit https://www.reddit.com/r/adventofcode/comments/rbj87a/comment/hnpad75/

(defn get-numbers [input]
  (let [frequency-patterns {"467889"  0
                            "89"      1
                            "47788"   2
                            "77889"   3
                            "6789"    4
                            "67789"   5
                            "467789"  6
                            "889"     7
                            "4677889" 8
                            "677889"  9}
        frequency-map (-> input first str/join frequencies)
        get-frequency (fn [x] (get frequency-map x))
        to-freq-pattern (fn [v] (->> v (map #(map get-frequency %)) (map sort) (map str/join)))
        digit-from-freq-pattern (fn [v] (get frequency-patterns v))]
    (->> input second to-freq-pattern (map digit-from-freq-pattern) (str/join) (parse-int))))

(defn part-01 [input]
  (let [allowed-size-filter (fn [x] (some #{(count x)} '(2 4 3 7)))]
    (->> input (map second) flatten (filter allowed-size-filter) count)))

(defn part-02 [input]
  (->> input (map get-numbers) (reduce +)))

(defn -main [& _]
  (println "Day 08:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
