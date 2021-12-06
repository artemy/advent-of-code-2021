(ns day_06
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_06.txt")

(def data (let [split-by-comma #(str/split % #",")]
            (->> input-file open-resource first split-by-comma (map parse-int))))

(defn part-01 [input days]
  (let [merge-overlapping-ages (fn [m]
                                 (merge-with +
                                             (dissoc m -1)
                                             {6 (get-or-default m -1 0)
                                              8 (get-or-default m -1 0)}))]
    (loop [fish (->> input
                     (group-by identity)
                     (map-vals count))
           day 1]
      (let [updated-fish (->> fish
                              (map-keys dec)
                              merge-overlapping-ages)]
        (if (< day days)
          (recur updated-fish (inc day))
          (->> updated-fish vals (reduce +)))))))

(defn -main [& _]
  (println "Part 1: " (part-01 data 80))
  (println "Part 2: " (part-01 data 256)))
