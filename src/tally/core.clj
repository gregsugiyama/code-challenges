(ns tally.core)

;; A theater sells popcorn & soda for $7 & $2.50 respectively. If bought together, the customer gets a deal ($9 total).
;; The deal is only effective for purchases from the same day.

;; for example, ["2021-03-04","soda"
;;               "2021-04-04","soda"
;;               "2021-04-04","popcorn"
;;               "2021-04-04","popcorn"
;;               "2021-05-04","soda"
;;               "2021-05-04","popcorn"]
;;                => total: $27.50

;; Write a function that will tally up a collection of purchaces

(def purchases ["2021-03-04,soda"
                "2021-04-04,soda"
                "2021-04-04,popcorn"
                "2021-04-04,popcorn"
                "2021-05-04,soda"
                "2021-05-04,popcorn"])

;; Step 1: Parse the input string into tuples of [date item]

(defn parse-purchase
  [purchase]
  (clojure.string/split purchase #","))

;; Step 2: Categorize the parsed sales items by date of purchase 

(defn get-sales-by-date
  [purchases]
  (reduce (fn [acc p]
            (let [[date item] (parse-purchase p)]
              (if (get acc date)
                (update acc date conj item)
                (assoc acc date [item])))) {} purchases))

;; Step 3: Tally the sales

(defn get-total-sales
  [sales-by-date]
  (->> (map (fn [[_ v]]
              (let [sales         (frequencies v)
                    soda-sales    (get sales "soda")
                    popcorn-sales (get sales "popcorn")]
                (cond
                  (nil? soda-sales)
                  (* popcorn-sales 7) ;; if there are only soda sales, return the total value of soda sales

                  (nil? popcorn-sales)
                  (* soda-sales 2.5) ;; if there are only popcorn sales, return the total value of popcorn sales

                  (= soda-sales popcorn-sales)
                  (* soda-sales 9) ;; if the number of sales between popcorn & soda are equal, multiply by the combo pricing 

                  (> soda-sales popcorn-sales)
                  (+ (* popcorn-sales 9) (* (- soda-sales popcorn-sales) 2.5)) ;; if the total soda sales are greater then popcorn, multiply the lower value by the combo pricine & add to the value of remaining soda sales * soda price

                  (> popcorn-sales soda-sales)
                  (+ (* soda-sales 9) (* (- popcorn-sales soda-sales) 7)) ;; if the total popcorn sales are greater then soda, multiply the lower value by the combo pricine & add to the value of remaining popcorn sales * popcorn price
                  )))sales-by-date)
       (reduce +) ;; Sum the results for total sales
       ))

(comment
;; ANSWER
  (get-total-sales (get-sales-by-date purchases)) ;;-> 27.5
  )