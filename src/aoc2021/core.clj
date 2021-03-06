(ns aoc2021.core
  (:require [clojure.string :as str]
            [clojure.set]
            [clojure.edn :as edn]
            [clojure.set :as set]))

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

(def day8-individual-example [["acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"]
                              ["cdfeb" "fcadb" "cdfeb" "cdbaf"]])

(def day8-input (->> (slurp "resources/day8_input.txt") (str/split-lines)
                     (map (fn [s] (let [a (-> (subs s 0 (str/index-of s "|")) (#(str/split % #" ")))
                                        b (-> (subs s (inc (inc (str/index-of s "|")))) (#(str/split % #" ")))]
                                    [a b])))))

(defn day8-compute-output-value [[signalPatternsStr digits]]
  (let [signalPatterns (map set signalPatternsStr)
        one (first (filter #(= 2 (count %)) signalPatterns))
        seven (first (filter #(= 3 (count %)) signalPatterns))
        four (first (filter #(= 4 (count %)) signalPatterns))
        eight (first (filter #(= 7 (count %)) signalPatterns))
        six (first (filter #(and (= 6 (count %)) (= 1 (count (clojure.set/intersection % one)))) signalPatterns))
        three (first (filter #(and (= 5 (count %)) (= 2 (count (clojure.set/intersection % one)))) signalPatterns))
        two (first (filter #(and (= 5 (count %)) (= 2 (count (clojure.set/intersection % four)))) signalPatterns))
        five (first (filter #(and (= 5 (count %)) (not (#{two three} %))) signalPatterns))
        nine (first (filter #(and (= 6 (count %)) (empty? (clojure.set/difference three %))) signalPatterns))
        zero (first (filter #(and (= 6 (count %)) (not (#{six nine} %))) signalPatterns))
        digitsLst [zero one two three four five six seven eight nine]
        [digit1 digit2 digit3 digit4] (map #(.indexOf digitsLst %) (map set digits))]
    (+ (* 1000 digit1) (* 100 digit2) (* 10 digit3) digit4)))




(defn day8-part1 [inp]
  (let [seconds (apply concat (map second inp))
        unique_digits (filter (fn [s] (#{2 4 3 7} (count s))) seconds)]
    (count unique_digits)))

(defn day8-part2 [inp]
  (apply + (map day8-compute-output-value inp)))

;-------------------

(def day9-test-input (->> "2199943210
3987894921
9856789892
8767896789
9899965678" (str/split-lines) (map vec) (map (fn [l] (map #(Integer/parseInt (str %)) l))) (map vec) (vec)))

(def day9-input (->> (slurp "resources/day9_input.txt") (str/split-lines) (map vec) (map (fn [l] (map #(Integer/parseInt (str %)) l))) (map vec) (vec)))

(defn day9-low-point? [heightmap x y]
  (let [leftmost (= 0 x)
        rightmost (= (count (first heightmap)) (inc x))
        topmost (= 0 y)
        bottommost (= (count heightmap) (inc y))
        row (nth heightmap y)
        col (map #(nth % x) heightmap)]
    (and (or leftmost (< (nth row x) (nth row (- x 1))))
         (or topmost (< (nth col y) (nth col (- y 1))))
         (or rightmost (< (nth row x) (nth row (inc x))))
         (or bottommost (< (nth col y) (nth col (inc y)))))))

(defn day9-get-coords-around [heightmap [x y]]
  (let [ysize (count heightmap)
        xsize (count (first heightmap))
        points [[(- x 1) y] [(+ x 1) y] [x (- y 1)] [x (+ y 1)]]]
    (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< x xsize) (< y ysize))) points)))


(defn day9-get-higher-points [heightmap [x y]]
  (let [pointsAround (day9-get-coords-around heightmap [x y])
        currPoint (nth (nth heightmap y) x)
        higherPoints (filter (fn [[x1 y1]] (let [point (nth (nth heightmap y1) x1)] (and (not= 9 point) (> point currPoint)))) pointsAround)
        recurHigherPoints (apply concat (map #(day9-get-higher-points heightmap %) higherPoints))]
    (set (concat higherPoints recurHigherPoints))))


(defn day9-get-basin-size [heightmap point]
  (inc (count (day9-get-higher-points heightmap point))))



(defn day9-part1 [heightmap]
  (apply + (for [y (range (count heightmap))
                 x (range (count (first heightmap)))
                 :when (day9-low-point? heightmap x y)]
             (inc (nth (nth heightmap y) x)))))

(defn day9-part2 [heightmap]
  (let [lowPoints (for [y (range (count heightmap))
                        x (range (count (first heightmap)))
                        :when (day9-low-point? heightmap x y)]
                    [x y])
        basinSizes (map (partial day9-get-basin-size heightmap) lowPoints)
        threeLargestBasins (take 3 (reverse (sort basinSizes)))]
    (apply * threeLargestBasins)))

;-------------------

(def day10-test-input (->> "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]" (str/split-lines) (map (partial apply list))))

(def day10-input (->> (slurp "resources/day10_input.txt") (str/split-lines) (map (partial apply list))))

(defn day10-close-char [c]
  (condp = c
    \[ \]
    \( \)
    \{ \}
    \< \>))

(defn day10-open-char [c]
  (condp = c
    \] \[
    \) \(
    \} \{
    \> \<))

(defn day10-parse-line [line]
  (loop [currChar (first line)
         restOfLine (rest line)
         stack '()]
    (cond
      (and (not currChar) (empty? stack)) [:success nil]
      (and (not currChar) (seq stack)) [:incomplete stack]
      (and (#{\] \) \} \>} currChar) (= (day10-open-char currChar) (first stack))) (recur (first restOfLine)  (rest restOfLine) (rest stack))
      (and (#{\] \) \} \>} currChar) (not= (day10-open-char currChar) (first stack))) [:corrupted currChar]
      (#{\[ \( \{ \<} currChar) (recur (first restOfLine) (rest restOfLine) (conj stack currChar)))))

(defn day10-score-completion [completionLst]
  (loop [currChar (first completionLst)
         restOfLst (rest completionLst)
         total 0]
    (if currChar (recur (first restOfLst) (rest restOfLst) (+ (* 5 total) ({\( 1 \[ 2 \{ 3 \< 4} currChar)))
        total)))

(defn day10-part1 [lines]
  (let [parsedLines (map day10-parse-line lines)
        corruptedLines (filter #(= :corrupted (first %)) parsedLines)
        pointsMap {\) 3 \] 57 \} 1197 \> 25137}
        points (map #(pointsMap (second %)) corruptedLines)]
    (apply + points)))

(defn day10-part2 [lines]
  (let [parsedLines (map day10-parse-line lines)
        incompleteLines (filter #(= :incomplete (first %)) parsedLines)
        completionStrs (map second incompleteLines)
        scores (map day10-score-completion completionStrs)
        sortedScores (vec (sort scores))
        middleIndex (int (/ (count sortedScores) 2))]
    (sortedScores middleIndex)))

;-------------------

(def day11-test-input (->> "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526" (str/split-lines) (map vec) (map #(map (comp edn/read-string str) %)) (map vec) (vec)))

(def day11-example-input (->> "11111
19991
19191
19991
11111" (str/split-lines) (map vec) (map #(map (comp edn/read-string str) %)) (map vec) (vec)))

(def day11-input (->> (slurp "resources/day11_input.txt") (str/split-lines) (map vec) (map #(map (comp edn/read-string str) %)) (map vec) (vec)))

(defn day11-inc-at-coord [grid [r c]]
  (assoc grid r (assoc (nth grid r) c (+ 1 (nth (nth grid r) c)))))

(defn day11-inc-surrounding-coords [grid [r c]]
  (let [initCoords [[(- r 1) c] [(+ r 1) c] [r (- c 1)] [r (+ c 1)]
                    [(+ r 1) (+ c 1)] [(- r 1) (- c 1)] [(+ r 1) (- c 1)] [(- r 1) (+ c 1)]]
        coords (filter (fn [[r c]] (and (>= r 0) (>= c 0) (< r (count grid)) (< c (count (first grid))))) initCoords)]
    (reduce day11-inc-at-coord grid coords)))

(defn day11-perform-substep2 [grid]
  (let [allCoords (for [r (range (count grid)) c (range (count (first grid)))] [r c])
        coordsOver9 (filter (fn [[r c]] (< 9 (nth (nth grid r) c))) allCoords)]
    (loop [coordsOver9ToProcess coordsOver9 seenCoords (set coordsOver9) currGrid grid]
      (let [newGrid (reduce day11-inc-surrounding-coords currGrid coordsOver9ToProcess)
            newCoordsOver9 (filter (fn [[r c]] (< 9 (nth (nth newGrid r) c))) allCoords)
            newCoordsOver9ToProcess (filter #(not (seenCoords %)) newCoordsOver9)
            newSeenCoords (set/union seenCoords (set newCoordsOver9ToProcess))]
        (if (empty? newCoordsOver9ToProcess)
          newGrid
          (recur newCoordsOver9ToProcess newSeenCoords newGrid))))))
; Get all coords
; Find all coords > 9
; Reduce over them, adding to surrounding points
; Find coords all > 9 not seen, and recur at 2. If none, return.

(defn day11-perform-step [grid]
  (let [substep1grid (->> grid (map #(map (partial + 1) %)) (map vec) (vec))
        substep2grid (day11-perform-substep2 substep1grid)
        substep3grid (->> substep2grid (map #(map (fn [n] (if (> n 9) 0 n)) %)) (map vec) (vec))]
    substep3grid))

(defn day11-count-flashes [grid]
  (apply + (map #(count (filter (partial = 0) %)) grid)))


(defn day11-part1 [grid]
  (let [grids (take 101 (iterate day11-perform-step grid))
        flashes (map day11-count-flashes grids)]
    (apply + flashes)))

(defn day11-part2 [grid]
  (loop [step 0 grid grid]
    (let [newGrid (day11-perform-step grid)
          newStep (inc step)
          all-are-zero (every? (fn [row] (every? #(= 0 %) row)) newGrid)]
      (if all-are-zero newStep
          (recur newStep newGrid)))))

;-------------------

(def day12-test-input (->> "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"
                           (str/split-lines)
                           (map #(str/split % #"-"))
                           (reduce (fn [m [fst snd]] (update (update m snd (fnil conj #{} fst) nil) fst (fnil conj #{} snd) nil)) {})))

(def day12-input (->> (slurp "resources/day12_input.txt")
                      (str/split-lines)
                      (map #(str/split % #"-"))
                      (reduce (fn [m [fst snd]] (update (update m snd (fnil conj #{} fst) nil) fst (fnil conj #{} snd) nil)) {})))

(defn all-uppercase? [s]
  (every? #(Character/isUpperCase %) s))

(defn day12-get-paths-from [caveMap lst lstAsSet]
  (if (= "end" (last lst)) [lst]
      (let [lastPoint (last lst)
            newPoints (filter #(or (not (lstAsSet %)) (all-uppercase? %)) (caveMap lastPoint))]
        (apply concat (map #(day12-get-paths-from caveMap (conj lst %) (conj lstAsSet %)) newPoints)))))

(defn day12-get-extended-paths-from [caveMap lst lstAsSet]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result caveToTryTwice]
       (if (= "end" (last lst)) (rf result lst)
           (let [lastPoint (last lst)
                 newPointsFilter #(or (not (lstAsSet %))
                                      (all-uppercase? %)
                                      (and (= caveToTryTwice %)
                                           (= 1 (count (filter (partial = caveToTryTwice) lst)))))
                 newPoints (filter newPointsFilter (caveMap lastPoint))]
             (reduce
              (fn [result1 point]
                (((day12-get-extended-paths-from caveMap (conj lst point) (conj lstAsSet point))
                  rf)
                 result1 caveToTryTwice))
              result newPoints)))))))

(defn day12-part1 [caveMap]
  (count (day12-get-paths-from caveMap ["start"] #{"start"})))

(defn day12-part2 [caveMap]
  (let [cavesToTryTwice (->> (keys caveMap) (filter #(not (#{"start" "end"} %))) (remove all-uppercase?))
        getNumOfCavePaths (comp (day12-get-extended-paths-from caveMap ["start"] #{"start"})
                                (distinct)
                                (map (constantly 1)))]
    (transduce getNumOfCavePaths + 0 cavesToTryTwice)))

;-------------------

(defn day13-input-parser [inputStr]
  (let [[coords instrs] (str/split inputStr #"\n\n" 2)
        parsedCoords (map #(vec (map edn/read-string (str/split % #"," 2))) (str/split-lines coords))
        parsedInstrs (map (fn [[_ xy num]] [(keyword xy) (edn/read-string num)]) (re-seq #"(\w)=(\d+)" instrs))]
    [parsedCoords parsedInstrs]))

(def day13-test-input (day13-input-parser "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(def day13-input (day13-input-parser (slurp "resources/day13_input.txt")))

(defn day13-perform-fold [coords [xy axisPos]]
  (distinct (case xy
             :x (map (fn [coord] (if (< (first coord) axisPos) coord [(- axisPos (- (first coord) axisPos)) (second coord)])) coords)
             :y (map (fn [coord] (if (< (second coord) axisPos) coord [(first coord) (- axisPos (- (second coord) axisPos))])) coords))))

(defn day13-part1 [[coords instrs]]
  (count (day13-perform-fold coords (first instrs))))

(day13-perform-fold (first day13-test-input) (first (second day13-test-input)))
(day13-perform-fold [[6 0]] [:x 5])

(defn day13-part2 [[coords instrs]]
  (let [points (set (reduce day13-perform-fold coords instrs))
        maxx (apply max (map first points))
        maxy (apply max (map second points))]
    (with-out-str (doseq [y (range (inc maxy))
                          x (range (inc maxx))]
                   (when (= 0 x) (println))
                   (if (points [x y]) (print "#") (print "."))))))

;-------------------

(defn day14-parse-input [inputStr]
  (let [[templateStr pairStr] (str/split inputStr #"\n\n" 2)
        polymerTemplate (vec templateStr)
        pairRules (into {} (map (fn [[_ k v]] [k (first v)]) (re-seq #"(\w\w) -> (\w)" pairStr)))]
    [polymerTemplate pairRules]))

(def day14-test-input (day14-parse-input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))

(def day14-input (day14-parse-input (slurp "resources/day14_input.txt")))

(defn day14-perform-step [polymerTemplate pairRules]
  (loop [firstElem (first polymerTemplate)
         secondElem (second polymerTemplate)
         remElems (drop 2 polymerTemplate)
         accum [firstElem]]
    (if (not secondElem) accum
        (let [pair (str firstElem secondElem)
              accum1 (if-let [newElem (pairRules pair)] (conj accum newElem) accum)
              accum2 (conj accum1 secondElem)]
          (recur secondElem (first remElems) (drop 1 remElems) accum2)))))

(def day14-perform-recursive-step
  (memoize (fn [polymerTemplate pairRules n]
             (if (= n 1)
               (let [newTemplate (day14-perform-step polymerTemplate pairRules)
                     freqs (frequencies newTemplate)]
                 freqs)
               (let [newTemplate (day14-perform-step polymerTemplate pairRules)
                     pairsToRecurseOn (map (fn [a b] [a b]) newTemplate (rest newTemplate))
                     freqsToMinus (frequencies (rest (drop-last newTemplate)))
                     recursedFreqs (map #(day14-perform-recursive-step % pairRules (- n 1)) pairsToRecurseOn)
                     reducedFreqs (reduce (fn [a b] (merge-with + a b)) recursedFreqs)
                     finalFreqs (merge-with - reducedFreqs freqsToMinus)]
                 finalFreqs)))))

(defn day14-part1 [[polymerTemplate pairRules]]
  (let [finalPolymerTemplate (reduce (fn [oldTemplate _] (day14-perform-step oldTemplate pairRules)) polymerTemplate (range 10))
        freqs (frequencies finalPolymerTemplate)
        maxNumOfElem (apply max (vals freqs))
        minNumOfElem (apply min (vals freqs))]
    (- maxNumOfElem minNumOfElem)))

(defn day14-part2 [[polymerTemplate pairRules]]
  (let [freqs (day14-perform-recursive-step polymerTemplate pairRules 40)
        maxNumOfElem (apply max (vals freqs))
        minNumOfElem (apply min (vals freqs))]
    (- maxNumOfElem minNumOfElem)))
