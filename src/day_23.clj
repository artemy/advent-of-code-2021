(ns day_23)

; both parts are manually solved

; #############
; #...........#
; ###A#D#A#B###
;   #B#C#D#C#
;   #########
; start

; #############
; #A..........#
; ###.#D#A#B###
;   #B#C#D#C#
;   #########
; A + 3

; #############
; #AA.........#
; ###.#D#.#B###
;   #B#C#D#C#
;   #########
; A + 6

; #############
; #AA.B.......#
; ###.#D#.#B###
;   #.#C#D#C#
;   #########
; B + 3

; #############
; #A..B.......#
; ###.#D#.#B###
;   #A#C#D#C#
;   #########
; A + 3

; #############
; #...B.......#
; ###A#D#.#B###
;   #A#C#D#C#
;   #########
; A + 3

; #############
; #...B.....B.#
; ###A#D#.#.###
;   #A#C#D#C#
;   #########
; B + 2

; #############
; #...B.C...B.#
; ###A#D#.#.###
;   #A#C#D#.#
;   #########
; C + 5

; #############
; #...B.C...B.#
; ###A#D#.#.###
;   #A#C#.#D#
;   #########
; D + 6

; #############
; #...B.....B.#
; ###A#D#.#.###
;   #A#C#C#D#
;   #########
; C + 3

; #############
; #...B.....B.#
; ###A#.#.#D###
;   #A#C#C#D#
;   #########
; D + 6

; #############
; #...B.....B.#
; ###A#.#C#D###
;   #A#.#C#D#
;   #########
; C + 5

; #############
; #.........B.#
; ###A#.#C#D###
;   #A#B#C#D#
;   #########
; B + 3

; #############
; #...........#
; ###A#B#C#D###
;   #A#B#C#D#
;   #########
; B + 6

(defn part-01 []
  (+ (* 1 (+ 3 6 3 3))
     (* 10 (+ 3 2 3 6))
     (* 100 (+ 5 3 5))
     (* 1000 (+ 6 6))))

; #############
; #...........#
; ###A#D#A#B###
;   #D#C#B#A#
;   #D#B#A#C#
;   #B#C#D#C#
;   #########
; start

; #############
; #A..........#
; ###A#D#.#B###
;   #D#C#B#A#
;   #D#B#A#C#
;   #B#C#D#C#
;   #########
; A + 7

; #############
; #AB.........#
; ###A#D#.#B###
;   #D#C#.#A#
;   #D#B#A#C#
;   #B#C#D#C#
;   #########
; B + 7

; #############
; #AB........A#
; ###A#D#.#B###
;   #D#C#.#A#
;   #D#B#.#C#
;   #B#C#D#C#
;   #########
; A + 7

; #############
; #AB.B......A#
; ###A#D#.#.###
;   #D#C#.#A#
;   #D#B#.#C#
;   #B#C#D#C#
;   #########
; B + 6

; #############
; #AB.B.D....A#
; ###A#D#.#.###
;   #D#C#.#A#
;   #D#B#.#C#
;   #B#C#.#C#
;   #########
; D + 5

; #############
; #AB.B.D...AA#
; ###A#D#.#.###
;   #D#C#.#.#
;   #D#B#.#C#
;   #B#C#.#C#
;   #########
; A + 3

; #############
; #AB.B.D...AA#
; ###A#D#.#.###
;   #D#C#.#.#
;   #D#B#.#.#
;   #B#C#C#C#
;   #########
; C + 9

; #############
; #AB.B.D...AA#
; ###A#D#.#.###
;   #D#C#.#.#
;   #D#B#C#.#
;   #B#C#C#.#
;   #########
; C + 9

; #############
; #AB.B.....AA#
; ###A#D#.#.###
;   #D#C#.#.#
;   #D#B#C#.#
;   #B#C#C#D#
;   #########
; D + 7

; #############
; #AB.B.....AA#
; ###A#.#.#.###
;   #D#C#.#.#
;   #D#B#C#D#
;   #B#C#C#D#
;   #########
; D + 8

; #############
; #AB.B.....AA#
; ###A#.#.#.###
;   #D#.#C#.#
;   #D#B#C#D#
;   #B#C#C#D#
;   #########
; C + 6

; #############
; #AB.B...B.AA#
; ###A#.#.#.###
;   #D#.#C#.#
;   #D#.#C#D#
;   #B#C#C#D#
;   #########
; B + 6

; #############
; #AB.B...B.AA#
; ###A#.#C#.###
;   #D#.#C#.#
;   #D#.#C#D#
;   #B#.#C#D#
;   #########
; C + 7

; #############
; #AB.....B.AA#
; ###A#.#C#.###
;   #D#.#C#.#
;   #D#.#C#D#
;   #B#B#C#D#
;   #########
; B + 5

; #############
; #AB.......AA#
; ###A#.#C#.###
;   #D#.#C#.#
;   #D#B#C#D#
;   #B#B#C#D#
;   #########
; B + 6

; #############
; #A........AA#
; ###A#.#C#.###
;   #D#B#C#.#
;   #D#B#C#D#
;   #B#B#C#D#
;   #########
; B + 5

; #############
; #AA.......AA#
; ###.#.#C#.###
;   #D#B#C#.#
;   #D#B#C#D#
;   #B#B#C#D#
;   #########
; A + 2

; #############
; #AA.......AA#
; ###.#.#C#.###
;   #.#B#C#D#
;   #D#B#C#D#
;   #B#B#C#D#
;   #########
; D + 10

; #############
; #AA.......AA#
; ###.#.#C#D###
;   #.#B#C#D#
;   #.#B#C#D#
;   #B#B#C#D#
;   #########
; D + 10

; #############
; #AA.......AA#
; ###.#B#C#D###
;   #.#B#C#D#
;   #.#B#C#D#
;   #.#B#C#D#
;   #########
; B + 7

; #############
; #A........AA#
; ###.#B#C#D###
;   #.#B#C#D#
;   #.#B#C#D#
;   #A#B#C#D#
;   #########
; A + 5

; #############
; #.........AA#
; ###.#B#C#D###
;   #.#B#C#D#
;   #A#B#C#D#
;   #A#B#C#D#
;   #########
; A + 5

; #############
; #..........A#
; ###.#B#C#D###
;   #A#B#C#D#
;   #A#B#C#D#
;   #A#B#C#D#
;   #########
; A + 9

; #############
; #...........#
; ###.#B#C#D###
;   #A#B#C#D#
;   #A#B#C#D#
;   #A#B#C#D#
;   #########
; A + 9

(defn part-02 []
  (+ (* 1 (+ 7 7 3 2 5 5 9 9))
     (* 10 (+ 7 6 6 5 6 5 7))
     (* 100 (+ 9 9 6 7))
     (* 1000 (+ 5 7 8 10 10))))

(defn -main [& _]
  (println "Day 23:")
  (println "\t-> Part 1: " (part-01))
  (println "\t-> Part 2: " (part-02)))
