(ns day_24
  (:require [utils :refer [open-resource parse-int split-whitespace]]))

(def input-file "day_24.txt")

(def data (-> input-file
              open-resource))

(defn prepare-data
  [input]
  (let [parse-arguments (fn [arg]
                          (if (every? #(Character/isLetter ^char %) arg)
                            (keyword arg)
                            (parse-int arg)))
        parse-command (fn [[cmd & args]]
                        (conj (map parse-arguments args) (keyword cmd)))]

    (->> input
         (map split-whitespace)
         (map parse-command))))

; based on https://old.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hpuxf5l/

(defn get-next-commands [commands digit-index]
  (let [offset-for-current-digit (fn [command-index] (+ command-index (* digit-index 18)))]
    (->> [4 5 15]
         (map offset-for-current-digit)
         (map #(nth commands %))
         (map last))))

(defn find-number [digits commands]
  (loop [digits digits
         digit-index (range 0 14)
         stack '()]
    (if (empty? digit-index)
      digits
      (let [[current-digit-index & rest-digit-index] digit-index
            [div chk add] (get-next-commands commands current-digit-index)]
        (if (= 1 div)
          (recur digits rest-digit-index (conj stack [current-digit-index add]))
          (let [[[j add] & stack] stack
                digits (-> digits
                           (#(assoc % current-digit-index (+ (nth % j) add chk)))
                           (#(if (< 9 (nth % current-digit-index))
                               (-> %
                                   (update j - (nth % current-digit-index) -9)
                                   (assoc current-digit-index 9))
                               %))
                           (#(if (> 1 (nth % current-digit-index))
                               (-> %
                                   (update j - (nth % current-digit-index) -1)
                                   (assoc current-digit-index 1))
                               %)))]
            (recur
              digits
              rest-digit-index
              stack)))))))

(defn part-01 [input]
  (->> input
       prepare-data
       (find-number [9 9 9 9 9 9 9 9 9 9 9 9 9 9])
       (apply str)))

(defn part-02 [input]
  (->> input
       prepare-data
       (find-number [1 1 1 1 1 1 1 1 1 1 1 1 1 1])
       (apply str)))

(defn -main [& _]
  (println "Day 24:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
