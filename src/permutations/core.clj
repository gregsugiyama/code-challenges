(ns permutations.core
  (:require [clojure.test :refer [deftest is testing]]))

(defn char-from-index-to-position
  [input i p]
  (cond
    (= i p) input
    (< i p) (str (subs input 0 i)
                 (subs input (inc i) (inc p))
                 (nth input i)
                 (subs input (inc p)))
    (> i p) (str (subs input 0 p)
                 (nth input i)
                 (subs input p i)
                 (subs input (inc i)))))

(defn super-permute-str
  [input]
  (loop [index          0
         position       0
         perm-set       #{}]
    (let [should-recur?          (< index (count input)) ;; 3
          should-reset-position? (= position (dec (count input)))
          current-perm           (when should-recur?
                                   (char-from-index-to-position input index position))
          updated-perm-set       (if should-recur?
                                   (conj perm-set current-perm)
                                   perm-set)
          updated-position       (if should-reset-position?
                                   0
                                   (inc position))
          updated-index         (if should-reset-position?
                                  (inc index)
                                  index)]
      (println index position current-perm)
      (if-not should-recur?
        updated-perm-set
        (recur updated-index updated-position updated-perm-set)))))

(defn dissoc-nth-str
  "Provides the string `s` without the character at `idx`,
  thus making `s` one character shorter.

  Seriously, why isn't `dissoc` supported for strings and vectors?"
  [s idx]
  (str (subs s 0 idx) (subs s (inc idx))))

(defn insert-nth-str
  "Provides the string `s` with the character `c` added at position `idx`.
  The previous character at `idx` is shifted up by one index."
  [s idx c]
  (str (subs s 0 idx) c (subs s idx)))

(defn permute-str
  "Provides the set of all permutations of the letters in the given string `s`."
  [s]
  (let [length (count s)]
    (if (<= length 1)
      (hash-set s)
      (let [indices (range length)]
        (set (mapcat (fn [index]
                       (let [c                (nth s index)
                             k-1-permutations (permute-str (dissoc-nth-str s index))]
                         (mapcat (fn [permutation]
                                   (let [indices        (range (count permutation))
                                         k-permutations (map (fn [index]
                                                               (insert-nth-str permutation index c))
                                                             indices)]
                                     k-permutations))
                                 k-1-permutations)))
                     indices))))))

(defn permute-str-first-try
  "Using pseudorandomness to approximate all permutations of the string `s`."
  [s]
  (set (repeatedly 100000 #(apply str (shuffle (vec s))))))

(defn factorial
  "Calculates the factorial of nonnegative integer `n`;
  that is, the product of all integers from one up to `n`.
  The factorial of zero is defined to be one."
  [n]
  {:pre [(and (integer? n) (or (pos? n) (zero? n)))]}
  (reduce * 1 (range 1 (inc n))))

(def short-strings
  ["cat" "dog" "an" "the" "test"])

(deftest permute-str-test
  (testing "All permutations have to contain exactly the same letters as the original string."
    (doseq [input short-strings]
      (let [letter-frequencies (comp frequencies vec)
            input-frequencies  (letter-frequencies input)
            expected           true
            actual             (every? #(= input-frequencies %)
                                       (map letter-frequencies
                                            (permute-str input)))]
        (is (= expected actual)))))
  (testing "The number of permutations is a known function of the string's length and the number of duplicated letters in it."
    (doseq [input short-strings]
      (let [input-letters         (vec input)
            n                     (count input-letters)
            numbers-of-duplicates (frequencies input-letters)
            expected              (/ (factorial n)
                                     (reduce * 1 (map factorial (vals numbers-of-duplicates))))
            actual                (count (set (permute-str input)))]
        (is (= expected actual))))))

(deftest super-permute-str-test
  (testing "All permutations have to contain exactly the same letters as the original string."
    (doseq [input short-strings]
      (let [letter-frequencies (comp frequencies vec)
            input-frequencies  (letter-frequencies input)
            expected           true
            actual             (every? #(= input-frequencies %)
                                       (map letter-frequencies
                                            (super-permute-str input)))]
        (is (= expected actual)))))
  (testing "The number of permutations is a known function of the string's length and the number of duplicated letters in it."
    (doseq [input short-strings]
      (let [input-letters         (vec input)
            n                     (count input-letters)
            numbers-of-duplicates (frequencies input-letters)
            expected              (/ (factorial n)
                                     (reduce * 1 (map factorial (vals numbers-of-duplicates))))
            actual                (count (set (super-permute-str input)))]
        (is (= expected actual))))))


(comment
  (clojure.test/run-tests)

  (->> (range)
       (map factorial)
       (take 10))

  (permute-str "cat")
  (permute-str "test")



  (super-permute-str "cat")

  (defn trade-positions
    [p1 p2]
    (let []))

  (defn foo-permute-str
    [input]
    (loop [index          0
           position       0
           perm-set       #{}]
      (let [should-recur?          (< index (count input))
            should-reset-position? (= position (dec (count input)))
            current-perm           (when should-recur?
                                     (char-from-index-to-position input index position))
            updated-perm-set       (if should-recur?
                                     (conj perm-set current-perm)
                                     perm-set)
            updated-position       (if should-reset-position?
                                     0
                                     (inc position))
            updated-index         (if should-reset-position?
                                    (inc index)
                                    index)]
        (println index position current-perm)
        (if-not should-recur?
          updated-perm-set
          (recur updated-index updated-position updated-perm-set))))))
