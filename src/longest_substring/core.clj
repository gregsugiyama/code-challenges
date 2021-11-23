(ns longest-substring.core)

;; Given a string s, find the length of the longest substring without repeating characters.

;; Example 1:

;; Input: s = "abcabcbb"
;; Output: 3
;; Explanation: The answer is "abc", with the length of 3.
;; Example 2:

;; Input: s = "bbbbb"
;; Output: 1
;; Explanation: The answer is "b", with the length of 1.
;; Example 3:

;; Input: s = "pwwkew"
;; Output: 3
;; Explanation: The answer is "wke", with the length of 3.
;; Notice that the answer must be a substring, "pwke" is a subsequence and not a substring.
;; Example 4:

;; Input: s = ""
;; Output: 0


;; Constraints:

;; 0 <= s.length <= 5 * 104
;; s consists of English letters, digits, symbols and spaces.

(defn find-longest-substring
  [input]
  (let [input-vec               (clojure.string/split input #"")
        replace-longest-substr? (fn [longest current]
                                  (> (count current)
                                     (count longest)))
        longest-substr          (fn [longest current]
                                  (if (replace-longest-substr? longest current)
                                    current
                                    longest))]
    (loop [string-seq input-vec
           longest    ""
           current    []]
      (println string-seq longest current)
      (cond
        (empty? string-seq)
        {:longest-substr (apply str longest)
         :length         (count longest)}

        (some #(= % (first string-seq)) current)
        (recur string-seq (longest-substr longest current) [])

        :else
        (recur (next string-seq) (longest-substr longest current) (conj current (first string-seq)))))))

(comment
  (find-longest-substring "abcabcbb")
  (find-longest-substring "bbbbb")
  (find-longest-substring "pwwkew")
  (find-longest-substring ""))