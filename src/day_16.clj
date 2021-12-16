(ns day_16
  (:require [clojure.string :as str]
            [utils :refer [open-resource]]))

(def input-file "day_16.txt")

(def data (->> input-file
               open-resource))

(defn binary-to-int [bs] (Integer/parseInt bs 2))

(defn char-to-binary [ch]
  (-> ch (#(Character/getNumericValue ^char %))
      (Integer/toBinaryString)
      (#(format "%4s" %))
      (str/replace #" " "0")))

(defn to-binary-string [in] (->> in (map char-to-binary) (apply str)))

(defn parse-value-packet
  [input]
  (let [number-parts (fn [in]
                       (loop [remainder in
                              parts []]
                         (let [group (subs remainder 0 1)
                               bits (subs remainder 1 5)]
                           (if (= group "0")
                             (conj parts bits)
                             (recur (subs remainder 5) (conj parts bits))))))]
    (-> input number-parts
        (#(assoc {} :length (-> % count (* 5) (+ 6))
                    :number (-> % str/join (Long/parseLong 2)))))))

(defn header [in]
  {:version     (-> in (subs 0 3) binary-to-int)
   :packet-type (-> in (subs 3 6) binary-to-int)})

(defn parse-packet
  [in]
  (let [hdr (header in)
        parse-length-packet (fn [in]
                              (let [length-total (-> in (subs 0 15) binary-to-int)]
                                (loop [remainder (subs in 15)
                                       packets []]
                                  (if (= (->> packets (map :length) (reduce +)) length-total)
                                    {:packets packets :length (+ 7 15 length-total)}
                                    (let [packet (parse-packet remainder)]
                                      (recur (subs remainder (:length packet)) (conj packets packet)))))))
        parse-number-packet (fn [in]
                              (let [subpackets-total (-> in (subs 0 11) binary-to-int)]
                                (loop [remainder (subs in 11)
                                       packets []]
                                  (if (= (count packets) subpackets-total)
                                    {:packets packets :length (+ 7 11 (reduce + (map :length packets)))}
                                    (let [packet (parse-packet remainder)]
                                      (recur (subs remainder (:length packet)) (conj packets packet))))
                                  )))
        parse-operator-packet (fn [in]
                                (let [length-type (binary-to-int (subs in 0 1))
                                      remaining (subs in 1)
                                      result (if (= 0 length-type)
                                               (parse-length-packet remaining)
                                               (parse-number-packet remaining))]
                                  (merge {:length-type length-type} result)))]
    (merge hdr
           (if (= 4 (:packet-type hdr))
             (parse-value-packet (subs in 6))
             (parse-operator-packet (subs in 6))))))

(defn version-counter [packet]
  (let [get-packets-or-empty (fn [m] (get m :packets []))]
    (reduce
      (fn [p n]
        (->> n get-packets-or-empty (map version-counter) (reduce + (+ p (:version n)))))
      0 [packet])))

(def bool-to-int {false 0 true 1})

(def type-id-to-operator {0 #(apply + %)
                          1 #(apply * %)
                          2 #(apply min %)
                          3 #(apply max %)
                          4 identity
                          5 #(get bool-to-int (apply > %))
                          6 #(get bool-to-int (apply < %))
                          7 #(get bool-to-int (apply = %))})

(defn packet-calculator [packet]
  (if (contains? packet :number)
    (:number packet)
    ((get type-id-to-operator (:packet-type packet))
     (map packet-calculator (:packets packet)))))


(defn part-01 [input]
  (->> input first to-binary-string parse-packet version-counter))

(defn part-02 [input]
  (->> input first to-binary-string parse-packet packet-calculator))

(defn -main [& _]
  (println "Day 16:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
