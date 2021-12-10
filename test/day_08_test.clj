(ns day_08-test
  (:require [clojure.test :refer [deftest is]]
            [day_08 :refer [part-01 part-02]]))

(def test-data
  '((("be" "cfbegad" "cbdgef" "fgaecd" "cgeb" "fdcge" "agebfd" "fecdb" "fabcd" "edb") ("fdgacbe" "cefdb" "cefbgd" "gcbe"))
    (("edbfga" "begcd" "cbg" "gc" "gcadebf" "fbgde" "acbgfd" "abcde" "gfcbed" "gfec") ("fcgedb" "cgb" "dgebacf" "gc"))
    (("fgaebd" "cg" "bdaec" "gdafb" "agbcfd" "gdcbef" "bgcad" "gfac" "gcb" "cdgabef") ("cg" "cg" "fdcagb" "cbg"))
    (("fbegcd" "cbd" "adcefb" "dageb" "afcb" "bc" "aefdc" "ecdab" "fgdeca" "fcdbega") ("efabcd" "cedba" "gadfec" "cb"))
    (("aecbfdg" "fbg" "gf" "bafeg" "dbefa" "fcge" "gcbea" "fcaegb" "dgceab" "fcbdga") ("gecf" "egdcabf" "bgf" "bfgea"))
    (("fgeab" "ca" "afcebg" "bdacfeg" "cfaedg" "gcfdb" "baec" "bfadeg" "bafgc" "acf") ("gebdcfa" "ecba" "ca" "fadegcb"))
    (("dbcfg" "fgd" "bdegcaf" "fgec" "aegbdf" "ecdfab" "fbedc" "dacgb" "gdcebf" "gf") ("cefg" "dcbef" "fcge" "gbcadfe"))
    (("bdfegc" "cbegaf" "gecbf" "dfcage" "bdacg" "ed" "bedf" "ced" "adcbefg" "gebcd") ("ed" "bcgafe" "cdgba" "cbgef"))
    (("egadfb" "cdbfeg" "cegd" "fecab" "cgb" "gbdefca" "cg" "fgcdab" "egfdb" "bfceg") ("gbdfcae" "bgc" "cg" "cgb"))
    (("gcafb" "gcf" "dcaebfg" "ecagb" "gf" "abcdeg" "gaef" "cafbge" "fdbac" "fegbdc") ("fgae" "cfgab" "fg" "bagce"))))

(deftest part-01-test
  (let [expected 26]
    (is (= (part-01 test-data) expected))))

(deftest part-02-test
  (let [expected 61229]
    (is (= (part-02 test-data) expected))))
