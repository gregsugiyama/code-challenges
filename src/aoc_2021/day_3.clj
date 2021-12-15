(ns aoc-2021.day-3
  (:require
   [clojure.java.io :as io]))

(def input (-> (io/resource "aoc_2021/day_3.txt")
               (io/reader)
               (line-seq)))

(defn gamma-rate
  [input]
  (for [n (range (count (first input)))
        :let [parsed-input (for [s       input
                                 :let [num-vec (clojure.string/split s #"")]]
                             (mapv #(java.lang.Integer/parseInt %) num-vec))]]
    (->> parsed-input
         (group-by #(nth % n))
         (map (fn [[k v]]
                [k (count v)]))
         (sort-by second)
         (last)
         (first))))

(defn epsilon-rate
  [input]
  (for [n (range (count (first input)))
        :let [parsed-input (for [s       input
                                 :let [num-vec (clojure.string/split s #"")]]
                             (mapv #(java.lang.Integer/parseInt %) num-vec))]]
    (->> parsed-input
         (group-by #(nth % n))
         (map (fn [[k v]]
                [k (count v)]))
         (sort-by second)
         (first)
         (first))))

(defn to-decimal
  [binary-seq]
  (loop [n (dec (count binary-seq))
         idx 0
         sum 0]
    (if (>= idx (count binary-seq))
      sum
      (recur (dec n) (inc idx) (+ sum (* (nth binary-seq idx)  (int (Math/pow 2 n))))))))

(defn get-oxygen-rate
  [input]
  (let [value-length (count (first input))
        parsed-input (for [s       input
                           :let [num-vec (clojure.string/split s #"")]]
                       (mapv #(java.lang.Integer/parseInt %) num-vec))]
    (loop [values parsed-input
           idx    0]
      (cond
        (or
         (= 1 (count values))
         (= idx value-length)) (first values)
        :else (let [groupings       (group-by #(nth % idx) values)
                    grouping-counts (into {} (map (fn [[k v]]
                                                    {k (count v)})) groupings)
                    new-values      (cond
                                      (=  (get grouping-counts 1)
                                          (get grouping-counts 0)) (get groupings 1)

                                      (>  (get grouping-counts 1)
                                          (get grouping-counts 0)) (get groupings 1)

                                      (>  (get grouping-counts 0)
                                          (get grouping-counts 1)) (get groupings 0))]
                (recur new-values (inc idx)))))))

(defn get-co2-rate
  [input]
  (let [value-length (count (first input))
        parsed-input (for [s       input
                           :let [num-vec (clojure.string/split s #"")]]
                       (mapv #(java.lang.Integer/parseInt %) num-vec))]
    (loop [values parsed-input
           idx    0]
      (cond
        (or
         (= 1 (count values))
         (= idx value-length)) (first values)
        :else (let [groupings       (group-by #(nth % idx) values)
                    grouping-counts (into {} (map (fn [[k v]]
                                                    {k (count v)})) groupings)
                    new-values      (cond
                                      (=  (get grouping-counts 1)
                                          (get grouping-counts 0)) (get groupings 0)

                                      (>  (get grouping-counts 1)
                                          (get grouping-counts 0)) (get groupings 0)

                                      (>  (get grouping-counts 0)
                                          (get grouping-counts 1)) (get groupings 1))]
                (recur new-values (inc idx)))))))

(comment
  ;; SOLUTION PT 1

  (* (to-decimal (epsilon-rate input)) (to-decimal (gamma-rate input)))

  ;; SOLUTION PT 2

  (* (to-decimal (get-oxygen-rate input)) (to-decimal (get-co2-rate input))))