(ns day_03
  (:require [clojure.string :as str]
            [utils :refer :all]))

(def input-file "day_03.txt")

(def data (let [split-chars #(str/split % #"")]
            (->> input-file
                 open-resource
                 (map split-chars)
                 (map #(map parse-int %)))))

(defn part-01 [input]
  (let [concat-by-frequency (fn [[epsilon gamma] freq] [(str epsilon (ffirst freq)) (str gamma (fsecond freq))])
        sort-by-value (fn [v] (sort-by val < v))]
    (->> input
         transpose
         (map frequencies)
         (map sort-by-value)
         (reduce concat-by-frequency [])
         (map parse-binary-int)
         (reduce *))))

(defn part-02 [input]
  (let [sort-by-frequencies (fn [v op] (->> v frequencies (sort-by val op)))
        filter-by-first-elements (fn [elements v] (filter #(= (take (count elements) %) elements) v))
        next-digit-frequencies (fn [elements filtered-data op] (nth (->> filtered-data
                                                                         transpose
                                                                         (map #(sort-by-frequencies % op)))
                                                                    (count elements)))
        find-next-digit (fn [element filtered-data op default]
                          (let [freqs (next-digit-frequencies element filtered-data op)]
                            (if (reduce not= (vals freqs))
                              (ffirst freqs)
                              default)))
        find-current-digits (fn [element op default]
                              (let [filtered-data (filter-by-first-elements element input)]
                                (if (> (count filtered-data) 1)
                                  (conj element (find-next-digit element filtered-data op default))
                                  (first filtered-data))
                                ))
        find-element-digits (fn [[oxygen co2] _]
                              [(find-current-digits oxygen > 1)
                               (find-current-digits co2 < 0)
                               ])]
    (->> input
         (reduce find-element-digits [[] []])
         (map str/join)
         (map parse-binary-int)
         (reduce *))))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
