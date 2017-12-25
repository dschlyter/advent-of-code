(ns aoc2017.day3
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

; day 3 part 1
; the nth ring contains (n/2+1)^2 entries minus all previous entries
; it is on the 302:th ring. It contains 2408 entries (603^2 - 601^2), each segment 602 entries, 301 entries in each half.
; Entry is 2082 entries from the end (603^2 - input), or 326 entries from the start, or 25 entries north of the middle.
; So 301 steps right and 25 steps north? - yep!

; day 3 part 2

; note: zero-indexed
(defn gen-spiral-path [n]
  (if (= n 0)
    {:x 0 :y 0}
    (concat
      (for [y (range (inc (- n)) (inc n))]
        {:x n :y y})
      (for [x (neg-range (dec n) (dec (- n)))]
        {:x x :y n})
      (for [y (neg-range (dec n) (dec (- n)))]
        {:x (- n) :y y})
      (for [x (range (inc (- n)) (inc n))]
        {:x x :y (- n)}))))

(defn spiral-path [n]
  (lazy-seq (concat (gen-spiral-path n) (spiral-path (inc n)))))

(defn squares-near [point]
  (for [x (range (dec (:x point)) (+ (:x point) 2))
        y (range (dec (:y point)) (+ (:y point) 2))]
    {:x x :y y}))

(defn spiral-value [point spiral]
  (->> (squares-near point)
       (map #(or (get spiral %) 0))
       (reduce +)))

(defn spiral-write [point spiral]
  (let [value (spiral-value point spiral)]
    {:last-value value :spiral (assoc spiral point value)}))

(defn spiral-values [spiral path]
  (let [write (spiral-write (first path) spiral)]
    (lazy-seq (cons
                (:last-value write)
                (spiral-values (:spiral write) (drop 1 path))))))

(defn day3-p2 []
  (first
    (drop-while #(< % 361527)
                (spiral-values {{:x 0 :y 0} 1} (spiral-path 1)))))

; (p (day3-p2))
