(ns aoc2017.day13
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day13-test (slurp "input/day13-test"))
(def day13-in (slurp "input/day13.txt"))

(defn day13-p1 [input]
  (->> input
       string/trim
       string/split-lines
       (map int-vec)
       (filter (fn [[d r]] (= 0 (mod d (+ r (max 0 (- r 2)))))))
       (map #(apply * %))
       (reduce +)))

(defn build-periods [input]
  (->> input
       string/trim
       string/split-lines
       (map int-vec)
       (map #(update % 1 (fn [n] (+ n (max 0 (- n 2))))))))

(defn is-caught [wait periods]
  (some (fn [[d p]] (= 0 (mod (+ d wait) p))) periods))

; (p (day13-p1 day13-test))
; (let [periods (build-periods day13-in)]
; (p (filter #(not (is-caught % periods)) (range 10000000))))
