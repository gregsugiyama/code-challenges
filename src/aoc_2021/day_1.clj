(ns aoc-2021.day-1)

(def input
  (->>
   (slurp "src/aoc_2021/day_1.txt")
   (clojure.string/split-lines)
   (map #(java.lang.Integer/parseInt %))))

(defn delta
  [numbers]
  (let [n1 (first numbers)
        n2 (second numbers)]
    (cond
      (= n1 n2) 0
      (> n1 n2) -1
      (> n2 n1) 1)))

(defn make-deltas
  [input]
  (let [vectors (partition 2 1 input)]
    (map delta vectors)))


;; --------------------- SOLUTION PT 1 ----------------------------

(def positive-deltas (->> (make-deltas input)
                          (filter pos?)
                          (count)))

;; ----------------------------------------------------------------

;; --------------------- SOLUTION PT 2 ----------------------------

(def sliding-scale-input (->> (partition 3 1 input)
                              (map #(apply + %))))

(def positive-deltas (->> (make-deltas sliding-scale-input)
                          (filter pos?)
                          (count)))

;; ----------------------------------------------------------------
(comment
  input)
