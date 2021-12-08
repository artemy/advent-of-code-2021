(ns day_08
  (:require [clojure.string :as str]
            [utils :refer :all]))

; this one is truly awful

(def input-file "day_08.txt")

(def data (let [split-by-pipe #(str/split % #"\|")]
            (->> input-file
                 open-resource
                 (map (comp #(map split-whitespace %)
                            #(map str/trim %)
                            split-by-pipe)))))

(defn decode-digits [s]
  (loop [in s
         deduced {}]
    (let [of-size (fn [s] #(->> % count (= s)))
          find-first-by-size (fn [l] (->> in (filter (of-size l)) first))
          has-length? (fn [i l] (some (of-size l) i))
          has-one? (fn [i d] (and (not (contains? d 2)) (has-length? i 2)))
          has-seven? (fn [i d] (and (not (contains? d 7)) (has-length? i 3)))
          has-four? (fn [i d] (and (not (contains? d 4)) (has-length? i 4)))
          has-eight? (fn [i d] (and (not (contains? d 8)) (has-length? i 7)))
          has-three? (fn [i d] (and (not (contains? d 3)) (has-length? i 5) (contains? d 7)))
          has-nine? (fn [i d] (and (not (contains? d 9)) (has-length? i 6) (contains? d 4)))
          has-zero? (fn [i d] (and (not (contains? d 0)) (has-length? i 6) (contains? d 7)))
          has-five? (fn [i d] (and (not (contains? d 5)) (has-length? i 5) (contains? d 9)))
          has-six? (fn [i d] (and (not (contains? d 6)) (->> i (filter (of-size 6)) count (= 1))))
          has-two? (fn [i d] (and (not (contains? d 2)) (->> i (filter (of-size 5)) count (= 1))))
          contains-all? (fn [x y] (every? (set x) (set y)))
          custom-filter (fn [l f] (->> in (filter (of-size l)) (filter f) first))]
      (cond
        ; 1
        (has-one? in deduced) (let [a (find-first-by-size 2)]
                                (recur (remove #{a} in) (merge deduced {1 a})))
        ; 4
        (has-four? in deduced) (let [a (find-first-by-size 4)]
                                 (recur (remove #{a} in) (merge deduced {4 a})))
        ; 7
        (has-seven? in deduced) (let [a (find-first-by-size 3)]
                                  (recur (remove #{a} in) (merge deduced {7 a})))
        ; 8
        (has-eight? in deduced) (let [a (find-first-by-size 7)]
                                  (recur (remove #{a} in) (merge deduced {8 a})))
        ; 9
        (has-nine? in deduced) (let [nine-filter (fn [x] (contains-all? x (get deduced 4)))
                                     nine (custom-filter 6 nine-filter)]
                                 (recur (remove #{nine} in) (merge deduced {9 nine})))
        ; 0
        (has-zero? in deduced) (let [zero-filter (fn [x] (contains-all? x (get deduced 7)))
                                     zero (custom-filter 6 zero-filter)]
                                 (recur (remove #{zero} in) (merge deduced {0 zero})))
        ; 6
        (has-six? in deduced) (let [a (find-first-by-size 6)]
                                (recur (remove #{a} in) (merge deduced {6 a})))
        ; 3
        (has-three? in deduced) (let [three-filter (fn [x] (contains-all? x (get deduced 7)))
                                      three (custom-filter 5 three-filter)]
                                  (recur (remove #{three} in) (merge deduced {3 three})))
        ; 5
        (has-five? in deduced) (let [five-filter (fn [x] (contains-all? (get deduced 9) x))
                                     five (custom-filter 5 five-filter)]
                                 (recur (remove #{five} in) (merge deduced {5 five})))
        ; 2
        (has-two? in deduced) (let [a (find-first-by-size 5)]
                                (recur (remove #{a} in) (merge deduced {2 a})))
        :else deduced))))

(defn get-numbers [in] (let [patterns (->> in first (map sort) (map str/join) sort decode-digits)
                             find-digit (fn [d] (->> patterns (filter (fn [[_ v]] (= (set d) (set v)))) ffirst))
                             convert-to-number (fn [i] (->> i (apply str) parse-int))]
                         (->> in second (map find-digit) convert-to-number)))

(defn part-01 [input]
  (let [allowed-size-filter (fn [x] (some #{(count x)} '(2 4 3 7)))]
    (->> input (map second) flatten (filter allowed-size-filter) count)))

(defn part-02 [input]
  (->> input (map get-numbers) (reduce +)))

(defn -main [& _]
  (println "Part 1: " (part-01 data))
  (println "Part 2: " (part-02 data)))
