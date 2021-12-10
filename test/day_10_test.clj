(ns day_10-test
  (:require [clojure.test :refer [deftest is]]
            [day_10 :refer [part-01 part-02]]))

(def test-data ["[({(<(())[]>[[{[]{<()<>>"
                "[(()[<>])]({[<{<<[]>>("
                "{([(<{}[<>[]}>{[]{[(<()>"
                "(((({<>}<{<{<>}{[]{[]{}"
                "[[<[([]))<([[{}[[()]]]"
                "[{[{({}]{}}([{[{{{}}([]"
                "{<[[]]>}<{[{[{[]{()[[[]"
                "[<(<(<(<{}))><([]([]()"
                "<{([([[(<>()){}]>(<<{{"
                "<{([{{}}[<[[[<>{}]]]>[]]"])

(deftest part-01-test
  (let [expected 26397]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 288957]
    (is (= (part-02 test-data) expected))))
