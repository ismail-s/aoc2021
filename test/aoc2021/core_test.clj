(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :refer :all]))

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
    (is (= 18605 (day5-part2 day5-input)))))
