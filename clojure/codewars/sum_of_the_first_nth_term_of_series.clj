; https://www.codewars.com/kata/sum-of-the-first-nth-term-of-series

(ns nthseries.core)

(defn series-sum [n]
  (->> (range n)
       (map #(/ (inc (* 3 %))))
       (reduce + 0.0)
       (format "%.2f")))
