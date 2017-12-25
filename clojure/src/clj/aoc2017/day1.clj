(ns aoc2017.day1
  (:require
    [aoc2017.utils :refer :all]
    [clojure.string :as string]))

(def d1input (string/trim (slurp "input/day1.txt")))

(defn day1-p1 [input]
  (->> input
       (#(string/split % #""))
       (map parse-int)
       (#(cons (last %) %))
       (partition 2 1)
       (filter #(apply = %)) (map first) (reduce +)))

(defn day1-p2 [input]
  (->> input
       (#(string/split % #""))
       (map parse-int)
       (#(let [half (/ (count %) 2)]
           (map vector % (concat (drop half %) (take half %)))))
       (filter #(apply = %)) (map first) (reduce +)))

; (p (day1-p1 d1input))
; (p (day1-p2 d1input))
