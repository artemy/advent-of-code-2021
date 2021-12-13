(ns day_12
  (:require [clojure.string :as str]
            [utils :refer [open-resource]]))

(def input-file "day_12.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [aggregate (fn [p [k v]] (update p k #(conj % v)))]
    (->> input
         (map #(str/split % #"-"))
         (#(concat % (map reverse %)))
         (reduce aggregate {}))))

(defn find-paths [max-lowercase-visits input]
  (let [lower-case? (fn [i] (= i (str/lower-case i)))
        already-visited-twice (fn [[_ visits]] (>= visits max-lowercase-visits))
        find-forbidden (fn [p] (->> p
                                    (filter lower-case?)
                                    frequencies
                                    (#(if (some already-visited-twice %)
                                        (keys %)
                                        ()))
                                    (into #{"start"})))
        dfs (fn [graph]
              (fn search
                [path]
                (let [current (peek path)]
                  (if (= "end" current)
                    [path]
                    (->> current graph
                         (remove (find-forbidden path))
                         (mapcat #(search (conj path %))))))))]
    ((dfs input) ["start"])))

(defn part-01 [input]
  (->> input prepare-data (find-paths 1) count))

(defn part-02 [input]
  (->> input prepare-data (find-paths 2) count))

(defn -main [& _]
  (println "Day 12:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
