(ns day_20
  (:require [clojure.string :as str]
            [utils :refer [open-resource parse-binary-int]]))

(def input-file "day_20.txt")

(def data (-> input-file
              open-resource))

(defn visualizer [input]
  (let [max-x (fn [v] (->> v keys (map first) (apply max) inc))
        min-x (fn [v] (->> v keys (map first) (apply min)))
        max-y (fn [v] (->> v keys (map second) (apply max) inc))
        min-y (fn [v] (->> v keys (map second) (apply min)))
        fill-grid (fn [v] (for [y (range (min-y v) (max-y v))
                                x (range (min-x v) (max-x v))]
                            (get v [x y] ".")))]
    (->> input
         fill-grid
         (partition (- (max-x input) (min-x input)))
         (map #(apply str %))
         (str/join "\n")
         println)))

(defn prepare-data
  [[raw-algorithm & raw-pixels]]
  (let [raw-pixels (remove empty? raw-pixels)
        extract-algorithm (fn [in] in)
        width (->> raw-pixels first count)
        build-coordinates (fn [i x] [[(rem i width) (quot i width)] x])
        extract-pixels (fn [in] (->> in str/join (map-indexed build-coordinates) (into {})))]
    {:algorithm (->> raw-algorithm extract-algorithm)
     :pixels    (->> raw-pixels extract-pixels)
     :iteration 0}))


(def adjacent-coord-modifiers [[-1 -1] [0 -1] [1 -1]
                               [-1 0] [0 0] [1 0]
                               [-1 1] [0 1] [1 1]])

(defn find-nearest-points [coordinate-map default]
  #(reduce (fn [p k] (str p (get coordinate-map k default)))
           ""
           (map (fn [x] (map + x %)) adjacent-coord-modifiers)))

(defn character-index [{:keys [algorithm pixels iteration]}]
  (fn [coords]
    (let [replace-with-digits (fn [in] (-> in (str/replace "." "0")
                                           (str/replace "#" "1")))
          default (if (= \. (first algorithm)) \. (if (odd? iteration) \# \.))
          ]
      (->> ((find-nearest-points pixels default) coords)
           replace-with-digits
           parse-binary-int
           (nth algorithm)))))

(defn enhance [input]
  (let [{:keys [pixels]} input
        max-x (fn [v] (->> v keys (map first) (apply max) (+ 2)))
        min-x (fn [v] (->> v keys (map first) (apply min) dec))
        max-y (fn [v] (->> v keys (map second) (apply max) (+ 2)))
        min-y (fn [v] (->> v keys (map second) (apply min) dec))
        get-grid (fn [v] (for [y (range (min-y v) (max-y v))
                               x (range (min-x v) (max-x v))]
                           [x y]))
        process-pixels (fn [p k] (assoc p k ((character-index input) k)))]
    (->> pixels
         get-grid
         (reduce process-pixels pixels)
         (assoc input :pixels)
         (#(update % :iteration inc)))))

(defn repeat-fn [f times]
  (fn [in] (nth (iterate f in) times)))

(defn part-01 [input]
  (->> input prepare-data ((repeat-fn enhance 2)) :pixels vals (remove #{\.}) count))

(defn part-02 [input]
  (->> input prepare-data ((repeat-fn enhance 50)) :pixels vals (remove #{\.}) count))

(defn -main [& _]
  (println "Day 20:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
