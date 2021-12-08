(ns aoc2021.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def day1-test-input (->> "199
200
208
210
200
207
240
269
260
263
" (str/split-lines) (map edn/read-string)))

(def day1-input (->> (slurp "resources/day1_input.txt") (str/split-lines) (map edn/read-string)))

(defn day1-part1 [nums]
  (apply + (map #(if (> %2 %1) 1 0) nums (drop 1 nums))))

(defn day1-part2 [nums]
  (apply + (map #(if (> (+ %2 %3 %4) (+ %1 %2 %3)) 1 0) nums (drop 1 nums) (drop 2 nums) (drop 3 nums))))

;-------------------

(def day2-test-input (->> "forward 5
down 5
forward 8
up 3
down 8
forward 2" (str/split-lines) (map #(str/split %1 #"\s")) (map (fn [[str num]] [(keyword str) (edn/read-string num)]))))

(def day2-input (->> (slurp "resources/day2_input.txt") (str/split-lines) (map #(str/split %1 #"\s")) (map (fn [[str num]] [(keyword str) (edn/read-string num)]))))


(defn day2-part1 [commands]
  (let [reducer (fn [[depth horiz] [command amount]]
                  (case command
                    :down [(+ amount depth) horiz]
                    :up [(- depth amount) horiz]
                    :forward [depth (+ amount horiz)]))
        res (reduce reducer [0 0] commands)]
    (apply * res)))

(defn day2-part2 [commands]
  (let [reducer (fn [[depth horiz aim] [command amount]]
                  (case command
                    :down [depth horiz (+ amount aim)]
                    :up [depth horiz (- aim amount)]
                    :forward [(+ (* aim amount) depth) (+ amount horiz) aim]))
        [depth horiz aim] (reduce reducer [0 0 0] commands)]
    (* depth horiz)))

;-------------------

(defn transpose [lst]
  (apply mapv vector lst))

(defn day3-get-most-common [{zero :0 one :1}]
  (if (> zero one) :0 :1))

(defn day3-get-least-common [{zero :0 one :1}]
  (if (< one zero) :1 :0))

(defn day3-keywords-to-num [keywords]
  (Integer/parseInt (apply str (map name keywords)) 2))

(def day3-test-input (->> "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
" (str/split-lines) (map #(str/split %1 #"")) (map #(map keyword %1))))

(def day3-input (->> (slurp "resources/day3_input.txt") (str/split-lines) (map #(str/split %1 #"")) (map #(map keyword %1))))

(defn day3-part1 [numStrs]
  (let [transposedLst (transpose numStrs)
        freqs (map frequencies transposedLst)
        most-common (map day3-get-most-common freqs)
        least-common (map day3-get-least-common freqs)
        most-common-as-num (day3-keywords-to-num most-common)
        least-common-as-num (day3-keywords-to-num least-common)]
    (* most-common-as-num least-common-as-num)))

(defn day3-iter-ratings [ratings indexToLookAt compFunc]
  (let [ratingsVec (map vec ratings)
        numToFilterOn (->> ratingsVec (map #(get % indexToLookAt)) (frequencies) (compFunc))]
    (filter #(= numToFilterOn (get % indexToLookAt)) ratingsVec)))

(defn day3-get-rating-by-comp [ratings compFunc]
  (loop [currRatings ratings
         i 0]
    (if (= 1 (count currRatings))
      (first currRatings)
      (let [newRatings (day3-iter-ratings currRatings i compFunc)]
        (recur newRatings (+ i 1))))))

(defn day3-part2 [numStrs]
  (let [o2_scrubber_rating_k (day3-get-rating-by-comp numStrs day3-get-most-common)
        co2_scrubber_rating_k (day3-get-rating-by-comp numStrs day3-get-least-common)
        o2_scrubber_rating (day3-keywords-to-num o2_scrubber_rating_k)
        co2_scrubber_rating (day3-keywords-to-num co2_scrubber_rating_k)]
    (* o2_scrubber_rating co2_scrubber_rating)))

;-------------------

(def day4-test-input
  (let [inp (str/split "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7" #"\n\n")
        randomNums (->> (first inp) (#(str/split % #",")) (map edn/read-string))
        matrices (->> (rest inp) (map str/split-lines)
                      (map (fn [a] (map (fn [b] (str/split (str/trim b) #" +")) a)))
                      (map (fn [m] (map (fn [l] (map (fn [s] [(edn/read-string s) nil]) l)) m))))]
    {:randomNums randomNums :matrices matrices}))

(def day4-input
  (let [inp (str/split (slurp "resources/day4_input.txt") #"\n\n")
        randomNums (->> (first inp) (#(str/split % #",")) (map edn/read-string))
        matrices (->> (rest inp) (map str/split-lines)
                      (map (fn [a] (map (fn [b] (str/split (str/trim b) #" +")) a)))
                      (map (fn [m] (map (fn [l] (map (fn [s] [(edn/read-string s) nil]) l)) m))))]
    {:randomNums randomNums :matrices matrices}))

(defn day4-mod-board [board randomNum]
  (map (fn [line]
         (map (fn [m]
                (if (= (first m) randomNum)
                  [(first m) 1]
                  m))
              line))
       board))

(defn day4-winning-board? [board]
  (if (or (some identity (map (fn [row] (every? identity (map second row))) board))
          (some identity (map (fn [col] (every? identity (map second col))) (transpose board))))
    board nil))

(defn day4-get-unmarked-nums-from-board [board]
  (apply concat (map (fn [row]
                       (map first (filter (fn [[n m]] (if (not m) n nil)) row)))
                     board)))

(defn day4-part1 [{randomNums :randomNums matrices :matrices}]
  (loop [currRandomNums randomNums currMatrices matrices]
    (let [currRandomNum (first currRandomNums)
          modBoards (map #(day4-mod-board % currRandomNum) currMatrices)
          winningBoard (some identity (map day4-winning-board? modBoards))]
          ;printWinningBoard (println winningBoard)]
      (if winningBoard
        (* currRandomNum (apply + (day4-get-unmarked-nums-from-board winningBoard)))
        (recur (rest currRandomNums) modBoards)))))

(defn day4-part2 [{randomNums :randomNums matrices :matrices}]
  (loop [currRandomNums randomNums currMatrices matrices]
    (let [currRandomNum (first currRandomNums)
          blow-up (if (= 0 (count currRandomNums)) (throw (Exception. "blah")))
          modBoards (map #(day4-mod-board % currRandomNum) currMatrices)
          winningBoards (filter day4-winning-board? modBoards)
          remainingBoards (remove day4-winning-board? modBoards)]
          ;dasfijdao (println "Winning boards:" winningBoards)
          ;asdjfkads (println "Remaing boards:" remainingBoards "\n")]
      (if (= 0 (count remainingBoards))
        (* currRandomNum (apply + (day4-get-unmarked-nums-from-board (first winningBoards))))
        (if (> 0 (count winningBoards))
          (if (or (= 0 (count remainingBoards)) (= 0 (count (rest currRandomNums))))
            ; (assert (= 1 (count winningBoards)))
            (* currRandomNum (apply + (day4-get-unmarked-nums-from-board (first winningBoards))))
            (recur (rest currRandomNums) remainingBoards))
          (recur (rest currRandomNums) remainingBoards))))))

;-------------------

(def day5-test-input (->> "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2" (str/split-lines) (map #(re-seq #"\d+" %)) (map #(map edn/read-string %))))

(def day5-input (->> (slurp "resources/day5_input.txt") (str/split-lines) (map #(re-seq #"\d+" %)) (map #(map edn/read-string %))))

(defn day5-straight-line? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn day5-get-points-of-line [[x1 y1 x2 y2]]
  (let [xinc (if (< x1 x2) 1 (if (> x1 x2) -1 0))
        yinc (if (< y1 y2) 1 (if (> y1 y2) -1 0))]
    (loop [res [[x1 y1]] x x1 y y1]
      (if (= [x y] [x2 y2]) res
          (let [newx (+ x xinc)
                newy (+ y yinc)
                newpoint [newx newy]]
            (recur (conj res newpoint) newx newy))))))


(defn day5-part1 [lst]
  (->> lst
       (filter day5-straight-line?)
       (map day5-get-points-of-line)
       (apply concat)
       (frequencies)
       (filter #(> (second %) 1))
       (count)))

(defn day5-part2 [lst]
  (->> lst
       (map day5-get-points-of-line)
       (apply concat)
       (frequencies)
       (filter #(> (second %) 1))
       (count)))

;-------------------

(def day6-test-input (edn/read-string (str "[" "3,4,3,1,2" "]")))

(def day6-input (edn/read-string (str "[" (slurp "resources/day6_input.txt") "]")))

(defn day6-simulate-a-day [ages]
  {8 (get ages 0 0)
   7 (get ages 8 0)
   6 (+ (get ages 7 0) (get ages 0 0))
   5 (get ages 6 0)
   4 (get ages 5 0)
   3 (get ages 4 0)
   2 (get ages 3 0)
   1 (get ages 2 0)
   0 (get ages 1 0)})

(defn day6-part1 [startAgesLst]
  (let [startAges (frequencies startAgesLst)
        simulations (iterate day6-simulate-a-day startAges)
        finalSimulation (nth simulations 80)
        total (apply + (map second finalSimulation))]
    total))

(defn day6-part2 [startAgesLst]
  (let [startAges (frequencies startAgesLst)
        simulations (iterate day6-simulate-a-day startAges)
        finalSimulation (nth simulations 256)
        total (apply + (map second finalSimulation))]
    total))

;-------------------

(def day7-test-input (edn/read-string (str "[" "16,1,2,0,4,2,7,1,2,14" "]")))

(def day7-input (edn/read-string (str "[" (slurp "resources/day7_input.txt") "]")))

(defn day7-get-fuel-for-position [horizPositionsMap position]
  (apply + (map (fn [[p n]] (* n (Math/abs (- p position))))
                horizPositionsMap)))

(defn day7-calc-fuel2 [n]
  (/ (* n (+ n 1)) 2))

(defn day7-get-fuel2-for-position [horizPositionsMap position]
  (apply + (map (fn [[p n]] (* n (day7-calc-fuel2 (Math/abs (- p position)))))
                horizPositionsMap)))

(defn day7-part1 [horizPositions]
  (let [minPosition (apply min horizPositions)
        maxPosition (apply max horizPositions)
        horizPositionsMap (frequencies horizPositions)]
    (->> (range minPosition (+ 1 maxPosition))
         (map (partial day7-get-fuel-for-position horizPositionsMap))
         (apply min))))

(defn day7-part2 [horizPositions]
  (let [minPosition (apply min horizPositions)
        maxPosition (apply max horizPositions)
        horizPositionsMap (frequencies horizPositions)]
    (->> (range minPosition (+ 1 maxPosition))
         (map (partial day7-get-fuel2-for-position horizPositionsMap))
         (apply min))))

;-------------------

(def day8-test-input (->> "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" (str/split-lines)
                          (map (fn [s] (let [a (-> (subs s 0 (str/index-of s "|")) (#(str/split % #" ")))
                                             b (-> (subs s (inc (inc (str/index-of s "|")))) (#(str/split % #" ")))]
                                         [a b])))))

(def day8-input (->> (slurp "resources/day8_input.txt") (str/split-lines)
                     (map (fn [s] (let [a (-> (subs s 0 (str/index-of s "|")) (#(str/split % #" ")))
                                        b (-> (subs s (inc (inc (str/index-of s "|")))) (#(str/split % #" ")))]
                                    [a b])))))

(defn day8-part1 [inp]
  (let [seconds (apply concat (map second inp))
        unique_digits (filter (fn [s] (#{2 4 3 7} (count s))) seconds)]
    (count unique_digits)))
