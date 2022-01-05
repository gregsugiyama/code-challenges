(ns aoc-2021.day-4)

(def numbers-to-draw (->> (clojure.string/split (slurp "src/aoc_2021/day_4_nums.txt") #",")
                          (mapv #(Integer/parseInt %))))

(def boards (->> (slurp "src/aoc_2021/day_4_boards.txt")
                 (clojure.string/split-lines)
                 (remove empty?)
                 (partition 5)))

(def parsed-boards (mapv (fn [board] (mapv (fn [s]
                                             (let [n-coll (remove empty? (clojure.string/split s #" "))]
                                               (mapv #(Integer/parseInt %) n-coll))) board)) boards))
(defn make-game-board
  [num-seq]
  (let [rows (partition 5 num-seq)]
    (mapv vec rows)))

(defn mark-number
  [n board]
  (into [] (for [row board]
             (mapv (fn [row-n]
                     (if (= row-n n)
                       :marked
                       row-n)) row))))

(defn find-bingo
  [n boards]
  (let [game-tally (mapv #(map (fn [row]
                                 (let [make-record (into {}
                                                         (map (juxt identity (constantly 0)) (range (count row))))]
                                   (reduce-kv (fn [record idx n]
                                                (if (= :marked n)
                                                  (update record idx inc)
                                                  record)) make-record row))) %) boards)
        bingos (remove nil? (map-indexed (fn [idx tally]
                                           (let [vertical-bingo (->> (apply merge-with + tally)
                                                                     (vals)
                                                                     (some #(= % 5)))
                                                 horizontal-bingo   (some true? (map (fn [row]
                                                                                       (let [five-in-a-row? (= 5 (->> (vals row)
                                                                                                                      (filter #(= 1 %))
                                                                                                                      (count)))]
                                                                                         five-in-a-row?)) tally))]
                                             (when (or
                                                    horizontal-bingo
                                                    vertical-bingo)
                                               {:winner idx
                                                :board (nth boards idx)
                                                :n n}))) game-tally))]
    bingos))

(defn bingo
  [numbers-to-draw game-boards]
  (loop
   [last-n nil
    num-list numbers-to-draw
    boards game-boards]
    (let [n              (first num-list)
          winning-boards (find-bingo last-n boards)]
      (cond
        (seq winning-boards) winning-boards
        (nil? n) "No Winner"
        :else (recur n (next num-list) (map #(mark-number n %) boards))))))

(defn solution-1
  [{:keys [board n] :as _bingo}]
  (let [unmarked-sum (->> board
                          flatten
                          (filter number?)
                          (apply +))]
    (* unmarked-sum n)))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn last-to-win-bingo
  [numbers-to-draw game-boards]
  (loop
   [last-n nil
    last-to-win nil
    num-list numbers-to-draw
    boards game-boards]
    (let [n              (first num-list)
          winning-boards (find-bingo last-n boards)]
      (println "LAST TO WIN" last-to-win)
      (cond
        (nil? n) last-to-win
        (seq winning-boards) (recur n (first winning-boards) (next num-list) (map #(mark-number n %) (vec-remove (:winner (first winning-boards)) (vec boards))))
        :else (recur n last-to-win (next num-list) (map #(mark-number n %) boards))))))

(comment
  (def nums [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1])
  (def boards [[[22 13 17 11  0]
                [8 2 23 4 24]
                [21 9 14 16 7]
                [6 10 3 18 5]
                [1 12 20 15 19]]

               [[3 15 0 2 22]
                [9 18 13 17 5]
                [19 8 7 25 23]
                [20 11 10 24 4]
                [14 21 16 12 6]]

               [[14 21 17 24 4]
                [10 16 15 9 19]
                [18 8 23 26 20]
                [22 11 13 6 5]
                [2 0 12 3 7]]])

  (solution-1 (first (last-to-win-bingo nums boards))))
