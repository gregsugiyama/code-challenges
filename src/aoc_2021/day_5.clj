(ns aoc-2021.day-5)

;; ===================  Part 1 ========================

;; For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.

(def input
  (->> (slurp "src/aoc_2021/day_5.txt")
       (clojure.string/split-lines)
       (map (fn [s]
              (let [[start end]  (clojure.string/split s #"->")
                    parsed-start (->> (clojure.string/split start #",")
                                      (mapv (comp #(java.lang.Integer/parseInt %) clojure.string/trim)))
                    parsed-end   (->> (clojure.string/split end #",")
                                      (mapv (comp #(java.lang.Integer/parseInt %) clojure.string/trim)))]
                [parsed-start parsed-end])))))

(def test-input
  '([[0 9] [5 9]]
    [[8 0] [0 8]]
    [[9 4] [3 4]]
    [[2 2] [2 1]]
    [[7 0] [7 4]]
    [[6 4] [2 0]]
    [[0 9] [2 9]]
    [[3 4] [1 4]]
    [[0 0] [8 8]]
    [[5 5] [8 2]]))

(defn is-horizontal-line?
  [line]
  (let [[start end] line
        [x1 _]     start
        [x2 _]     end]
    (= x1 x2)))

(defn is-vertical-line?
  [line]
  (let [[start end] line
        [_ y1]     start
        [_ y2]     end]
    (= y1 y2)))

(defn is-diagonal-line?
  [line]
  (let [[start end] line
        [x1 y1] start
        [x2 y2] end]
    (and (not= x1 x2)
         (not= y1 y2))))

(defn is-valid-line?
  [line]
  (or (is-horizontal-line? line)
      (is-vertical-line? line)))

;; Step 1 - Remove invalid lines

(def valid-lines
  (filter is-valid-line? input))

;; Step 2 - Draw lines

(defn draw-diagonal-line
  [line]
  (let [[start end] line
        [x1 y1]     start
        [x2 y2]     end
        min-x       (min x1 x2)
        max-x       (max x1 x2)
        min-y       (min y1 y2)
        max-y       (max y1 y2)
        x-range     (range min-x (inc max-x))
        y-range     (range min-y (inc max-y))
        xs           (if (neg? (- x2 x1))
                       (reverse x-range)
                       x-range)
        ys           (if (neg? (- y2 y1))
                       (reverse y-range)
                       y-range)]
    (mapv (fn [x y]
            [x y]) xs ys)))

(defn draw-line
  [line]
  (let [[start end]    (sort line)
        [x1 y1]        start
        [x2 y2]        end
        is-vertical?   (is-vertical-line? line)
        is-horizontal? (is-horizontal-line? line)
        is-diagonal?   (is-diagonal-line? line)]
    (cond
      is-vertical?   (->> (range x1 (inc x2))
                          (mapv #(vec [% y1])))
      is-horizontal? (->> (range y1 (inc y2))
                          (mapv #(vec [x1 %])))
      is-diagonal?   (draw-diagonal-line line))))

;; Step 3 - Draw all lines & find intersections

(defn find-line-overlaps
  [lines]
  (let [coords (mapcat identity
                       (map draw-line lines))]
    (count (filter (fn [[_ v]]
                     (> v 1)) (frequencies coords)))))

(comment
  ;; PT 1
  (find-line-overlaps valid-lines)

  ;; PT 2
  (find-line-overlaps input))