(ns day_18
  (:require [clojure.zip :as zip]
            [utils :refer [open-resource]]))

(def input-file "day_18.txt")

(def data (->> input-file
               open-resource))

(defn prepare-data [input] (->> input (map read-string)))

; inspired by https://gist.github.com/armstnp/2b0e368af6be95c342924636c0032c3a

(defn find-next-node [f node]
  (loop [nxt (zip/next node)]
    (cond
      (zip/end? nxt) false
      (f nxt) nxt
      :else (recur (zip/next nxt)))))

(defn find-right [node] (find-next-node #(->> % zip/node int?) node))

(defn find-left [node]
  (loop [node (zip/prev node)]
    (cond
      (nil? node) nil
      (int? (zip/node node)) node
      :else (recur (zip/prev node)))))

(defn explode [node]
  (let [[l r] (zip/node node)
        collapse-node (fn [node] (zip/replace node 0))
        collapse-left (fn [node]
                        (let [left-node (find-left node)]
                          (if left-node
                            (find-right (zip/edit left-node + l))
                            node)))
        collapse-right (fn [node]
                         (let [right-node (find-right node)]
                           (if right-node
                             (find-left (zip/edit right-node + r))
                             node)))]
    (->> node collapse-node collapse-left collapse-right)))

(defn split [node]
  (let [split-number (fn [n] (->> n (* 0.5)
                                  (#(vector (Math/floor %) (Math/ceil %)))
                                  (mapv int)))]
    (zip/edit node split-number)))

(defn find-exploder [node]
  (find-next-node
    #(and (->> % zip/node vector?) (->> % zip/path count (= 4)))
    node))

(defn find-splitter [node]
  (find-next-node
    #(and (->> % zip/node int?) (->> % zip/node (<= 10)))
    node))

(defn reduce-fish [root]
  (loop [current root]
    (let [exploder (find-exploder current)
          splitter (find-splitter current)]
      (cond
        exploder (recur (->> exploder explode zip/root zip/vector-zip))
        splitter (recur (->> splitter split zip/root zip/vector-zip))
        :else (zip/root current)))))

(defn sum-fish [left-fish right-fish]
  (->> [left-fish right-fish] zip/vector-zip reduce-fish))

(defn magnitude [x]
  (if (int? x)
    x
    (+ (* 3 (magnitude (first x)))
       (* 2 (magnitude (second x))))))

(defn part-01 [input]
  (->> input prepare-data (reduce sum-fish) magnitude))

(defn part-02 [input]
  (let [combinations (fn [in] (for [x in
                                    y in
                                    :when (not= x y)]
                                [x y]))]
    (->> input
         prepare-data
         combinations
         (map (fn [[x y]] (sum-fish x y)))
         (map magnitude)
         (apply max))))

(defn -main [& _]
  (println "Day 18:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
