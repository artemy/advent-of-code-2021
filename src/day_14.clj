(ns day_14
  (:require [clojure.string :as str]
            [utils :refer [open-resource parse-int split-by-comma]]))

(def input-file "day_14.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input]
  (let [[raw-template raw-rules] (split-with not-empty input)
        template (first raw-template)
        rules (->> raw-rules rest (map #(str/split % #"\s->\s")) (into {}))]
    {:template template
     :rules    rules}))

(defn apply-replacements [steps input]
  (let [{:keys [template rules]} input
        apply-insertion (fn [pair-frequencies [current-pair size]]
                          (-> pair-frequencies
                              (update current-pair #(- % size))
                              (update (str (first current-pair) (get rules current-pair)) (fnil #(+ size %) 0))
                              (update (str (get rules current-pair) (last current-pair)) (fnil #(+ size %) 0))))
        update-character-frequency (fn [chars [current-pair size]]
                                     (update chars (get rules current-pair) (fnil #(+ size %) 0)))
        apply-insertions (fn [[pair-frequencies char-frequencies] current-pair-frequency]
                           (if (contains? rules (first current-pair-frequency))
                             [(apply-insertion pair-frequencies current-pair-frequency)
                              (update-character-frequency char-frequencies current-pair-frequency)]
                             [pair-frequencies char-frequencies]))]
    (loop [[pair-frequencies char-frequencies] [(->> template
                                                     (partition 2 1)
                                                     (map #(apply str %))
                                                     frequencies)
                                                (->> template
                                                     (map str)
                                                     frequencies)]
           step 0]
      (if (>= step steps)
        char-frequencies
        (recur (reduce apply-insertions [pair-frequencies char-frequencies] pair-frequencies)
               (inc step))))))

(defn count-characters [steps input]
  (->> input
       (apply-replacements steps)
       vals
       ((juxt #(apply max %) #(apply min %)))
       (reduce -)))

(defn part-01 [input]
  (->> input prepare-data (count-characters 10)))

(defn part-02 [input]
  (->> input prepare-data (count-characters 40)))

(defn -main [& _]
  (println "Day 14:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
