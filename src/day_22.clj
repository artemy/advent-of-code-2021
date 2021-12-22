(ns day_22
  (:require [utils :refer [open-resource parse-int]]))

(def input-file "day_22.txt")

(def data (-> input-file
              open-resource))

(defn prepare-data
  [input]
  (let [parse-lines (fn [x]
                      (let [regex #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)"
                            [_ command & coords] (re-find regex x)]
                        (conj (->> coords (map parse-int) (partition 2)) (keyword command))))]
    (map parse-lines input)))

; based on https://old.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/hpizza8/

(defn process-commands [p [command x y z]]
  (let [process-coords (fn [[nx1 nx2] [ny1 ny2] [nz1 nz2]]
                         (fn [p [[[ex1 ex2] [ey1 ey2] [ez1 ez2]] sgn]]
                           (let [ix1 (max nx1 ex1)
                                 ix2 (min nx2 ex2)
                                 iy1 (max ny1 ey1)
                                 iy2 (min ny2 ey2)
                                 iz1 (max nz1 ez1)
                                 iz2 (min nz2 ez2)]
                             (if (and (<= ix1 ix2) (<= iy1 iy2) (<= iz1 iz2))
                               (update p [[ix1 ix2] [iy1 iy2] [iz1 iz2]] (fnil - 0) sgn)
                               p))))
        process-command (fn [command coords m]
                          (if (= :on command)
                            (update m coords (fnil inc 0))
                            m))]
    (->> p
         (reduce (process-coords x y z) p)
         (process-command command [x y z]))))

(defn calculate-sum [p [coords sgn]]
  (let [diff (fn [[fs sc]] (-> sc (- fs) inc))]
    (->> coords (map diff) (reduce * sgn) (+ p))))

(defn part-01 [input]
  (let [outside-allowed-boundaries? (fn [[_ & coords]]
                                      (some #(or (< % -50) (< 50 %)) (flatten coords)))]
    (->> input
         prepare-data
         (remove outside-allowed-boundaries?)
         (reduce process-commands {})
         (reduce calculate-sum 0))))

(defn part-02 [input]
  (->> input
       prepare-data
       (reduce process-commands {})
       (reduce calculate-sum 0)))

(defn -main [& _]
  (println "Day 22:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
