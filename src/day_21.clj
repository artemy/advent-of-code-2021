(ns day_21
  (:require [utils :refer [open-resource parse-int]]))

(def input-file "day_21.txt")

(def data (-> input-file
              open-resource))

(defn prepare-data
  [[player-1-raw player-2-raw]]
  (let [extract-position (fn [in] (let [[_ position] (re-find #"position: (\d+)" in)]
                                    (parse-int position)))]
    {:player1 {:score    0
               :position (extract-position player-1-raw)}
     :player2 {:score    0
               :position (extract-position player-2-raw)}
     :rolls   0}))

(def board (cycle (range 1 11)))
(defn next-position [position dice] (->> position (+ dice) dec (nth board)))

(defn game [input]
  (let [dice-roll (fn [roll] (->> (range 1 101) (cycle) (drop roll) (take 3) (apply +)))
        perform-move (fn [m player] (let [rolls (:rolls m)
                                          position (get-in m [player :position])
                                          new-position (->> (dice-roll rolls)
                                                            (next-position position))]
                                      (-> m
                                          (update-in [player :score] + new-position)
                                          (assoc-in [player :position] new-position)
                                          (update :rolls + 3))))]
    (loop [game input]
      (let [{:keys [player1 player2 rolls]} game
            current-player (if (even? rolls) :player1 :player2)]
        (if (or (<= 1000 (:score player1)) (<= 1000 (:score player2)))
          game
          (recur (-> game (perform-move current-player))))))))


(defn part-01 [input]
  (let [game-result (->> input prepare-data game)]
    (->> (min (get-in game-result [:player1 :score])
              (get-in game-result [:player2 :score]))
         (* (:rolls game-result)))))

; based on https://old.reddit.com/r/adventofcode/comments/rl6p8y/2021_day_21_solutions/hpe8pmy/

(def play (memoize
            (fn [pos1 pos2 score1 score2]
              (if (<= 21 score2)
                [0 1]
                (let [combinations [[3 1] [4 3] [5 6] [6 7] [7 6] [8 3] [9 1]]
                      next-move (fn [[wins1 wins2] [move n]]
                                  (->> (play pos2 (next-position pos1 move)
                                             score2 (+ score1 (next-position pos1 move)))
                                       reverse
                                       (map #(* n %))
                                       (map + [wins1 wins2])))]
                  (reduce next-move [0 0] combinations))))))

(defn part-02 [input]
  (let [game-result (->> input prepare-data)]
    (->> (play
           (get-in game-result [:player1 :position])
           (get-in game-result [:player2 :position]) 0 0)
         (apply max))))

(defn -main [& _]
  (println "Day 21:")
  (println "\t-> Part 1: " (part-01 data))
  (println "\t-> Part 2: " (part-02 data)))
