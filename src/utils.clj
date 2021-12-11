(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines split]]))

(defn open-resource [input-file]
  (let [data-file (io/resource input-file)]
    (->> data-file slurp split-lines)))

(defn ^{:doc      "Same as (first (second x))"
        :arglists '([x])
        :static   true}
  fsecond [x] (-> x second first))

(defn parse-int [v] (Integer/parseInt v 10))
(defn abs [x] (Math/abs ^int x))

(defn parse-binary-int [v] (Integer/parseInt v 2))

(defn transpose [input] (apply map list input))

(defn split-whitespace [v] (split v #"\s+"))

(defn map-vals [f m] (into {} (for [[k v] m] [k (f v)])))
(defn map-keys [f m] (into {} (for [[k v] m] [(f k) v])))

(defn find-middle [v] (nth v (-> v count (quot 2))))

(defn flatten-first [v] (mapcat identity v))
