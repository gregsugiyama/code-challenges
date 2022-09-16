(ns interval-list-intersections.core)

;; You are given two lists of closed intervals, firstList and secondList, where firstList [i] = [starti, endi] and secondList [j] = [startj, endj] . Each list of intervals is pairwise disjoint and in sorted order.

;; Return the intersection of these two interval lists.

;; A closed interval [a, b] (with a <= b) denotes the set of real numbers x with a <= x <= b.

;; The intersection of two closed intervals is a set of real numbers that are either empty or represented as a closed interval. For example, the intersection of [1, 3] and [2, 4] is [2, 3].

;; Example 1:

;; Input: firstList =  [[0,2],[5,10],[13,23],[24,25]]
;;        secondList = [[1,5],[8,12],[15,24],[25,26]]
;; Output: [[1,2],[5,5],[8,10],[15,23],[24,24],[25,25]]

;; Example 2:

;; Input: firstList = [[1,3],[5,9]], secondList = []
;; Output: []

;; Example 3:

;; Input: firstList = [], secondList = [[4,8],[10,12]]
;; Output: []

;; Example 4:

;; Input: firstList = [[1,7]], secondList = [[3,10]]
;; Output: [[3,7]]

;;*-----------------------------------------------------------*

(defn flatten-input
  [v1 v2]
  (apply
   concat
   (map (fn [n1 n2]
          (into [] [n1 n2])) v1 v2)))

(defn make-intervals
  [input]
  (partition 2 1 input))

(defn get-intersection
  [[interval-1 interval-2]]
  (let [[x1 y1] interval-1
        [x2 y2] interval-2
        x-intersection (when (>= x2 x1) x2)
        y-intersection (when (<= y1 y2) y1)]
    (when (<= x-intersection y-intersection)
      (->> [x-intersection y-intersection]
           (remove nil?)
           (vec)))))

(comment
  (def example-1 (flatten-input [[0 2] [5 10] [13 23] [24 25]]
                                [[1 5] [8 12] [15 24] [25 26]]))

  (def example-2 (flatten-input [[1 3] [5 9]] []))

  (def example-3 (flatten-input [] [[4 8] [10 12]]))

  (def example-4 (flatten-input [[1 7]] [[3 10]]))

  ;; Answers

  (vec (remove nil? (map get-intersection (make-intervals example-1))))
  (vec (remove nil? (map get-intersection (make-intervals example-2))))
  (vec (remove nil? (map get-intersection (make-intervals example-3))))
  (vec (remove nil? (map get-intersection (make-intervals example-4)))))


