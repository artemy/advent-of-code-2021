(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

(defn open-resource [input-file]
  (let [data-file (io/resource input-file)]
    (->> data-file slurp split-lines)))
