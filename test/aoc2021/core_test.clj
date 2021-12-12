(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

(defmacro tests-for-day [n res1t res1 res2t res2]
  (let [dayn-part1# (symbol (str "day" n "-part1"))
        dayn-part2# (symbol (str "day" n "-part2"))
        dayn-test-input# (symbol (str "day" n "-test-input"))
        dayn-input# (symbol (str "day" n "-input"))
        dayn-part1-s# (str "Day " n " Part 1")
        dayn-part2-s# (str "Day " n " Part 2")]
    `(do
       (testing ~dayn-part1-s#
         (is (= ~res1t (~dayn-part1# ~dayn-test-input#)))
         (is (= ~res1 (~dayn-part1# ~dayn-input#))))
       (testing ~dayn-part2-s#
        (is (= ~res2t (~dayn-part2# ~dayn-test-input#)))
        (is (= ~res2 (~dayn-part2# ~dayn-input#)))))))

(deftest aoc2021
  (testing "Day 1 Part 1"
    (is (= 7 (day1-part1 day1-test-input)))
    (is (= 1121 (day1-part1 day1-input))))
  (testing "Day 1 Part 2"
    (is (= 5 (day1-part2 day1-test-input)))
    (is (= 1065 (day1-part2 day1-input))))
  (testing "Day 2 Part 1"
    (is (= 150 (day2-part1 day2-test-input)))
    (is (= 1698735 (day2-part1 day2-input))))
  (testing "Day 2 Part 2"
    (is (= 900 (day2-part2 day2-test-input)))
    (is (= 1594785890 (day2-part2 day2-input))))
  (testing "Day 3 Part 1"
    (is (= 198 (day3-part1 day3-test-input)))
    (is (= 2972336 (day3-part1 day3-input))))
  (testing "Day 3 Part 2"
    (is (= 230 (day3-part2 day3-test-input)))
    (is (= 3368358 (day3-part2 day3-input))))
  (testing "Day 4 Part 1"
    (is (= 4512 (day4-part1 day4-test-input)))
    (is (= 2496 (day4-part1 day4-input))))
  (testing "Day 4 Part 2"
    (is (= 1924 (day4-part2 day4-test-input)))
    (is (= 25925 (day4-part2 day4-input))))
  (testing "Day 5 Part 1"
    (is (= 5 (day5-part1 day5-test-input)))
    (is (= 5197 (day5-part1 day5-input))))
  (testing "Day 5 Part 2"
    (is (= 12 (day5-part2 day5-test-input)))
    (is (= 18605 (day5-part2 day5-input))))
  (tests-for-day 6 5934 365131 26984457539 1650309278600)
  (tests-for-day 7 37 339321 168 95476244)
  (tests-for-day 8 26 512 61229 1091165)
  (tests-for-day 9 15 577 1134 1069200)
  (tests-for-day 10 26397 392097 288957 4263222782)
  (tests-for-day 11 1656 1546 195 471)
  (tests-for-day 12 19 4792 103 133360))
