(ns aoc-2021.day-2)

(def input (->> (slurp "src/aoc_2021/day_2.txt")
                (clojure.string/split-lines)
                (map (fn [line]
                       (let [[direction delta] (clojure.string/split line #" ")]
                         [direction (java.lang.Integer/parseInt delta)])))))

(def test-input '(["forward" 5] ["down" 5] ["forward" 8] ["up" 3] ["down" 8] ["forward" 2]))

(defn find-position
  [initial-pos input]
  (let [change-position (fn [[x y] [direction delta]]
                          (case direction
                            "forward" [(+ x delta) y]
                            "down"    [x (+ y delta)]
                            "up"      [x (- y delta)]))]
    (reduce change-position initial-pos input)))

(defn find-position-2
  [initial-pos input]
  (let [change-position (fn [[x y aim] [direction delta]]
                          (case direction
                            "forward" [(+ x delta) (+ y (* aim delta)) aim]
                            "down"    [x y (+ aim delta)]
                            "up"      [x y (- aim delta)]))]
    (reduce change-position initial-pos input)))

(comment
  ;; SOLUTION PT 1
  (apply * (find-position [0 0] input))

  ;; SOLUTION PT 2
  (apply * (drop-last (find-position-2 [0 0 0] test-input))))