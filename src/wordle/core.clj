(ns wordle.core
  (:require
   [clojure.string]))

(def word-list (->> (slurp "src/wordle/word_list.txt")
                    (clojure.string/split-lines)))

(comment
  (let  [foo 1
         bar     2])

  (print word-list)
  (take 10 word-list))
