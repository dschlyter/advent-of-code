(ns aoc2017.day15
  (:require
    [aoc2017.utils :refer :all]))

(def mersenne 2147483647)
(def iterations1 40000000)
(def iterations2 5000000)
; (def iterations 5)

(def first-16 (dec (bit-shift-left 1 16)))

(def day15-test [65 8921])
(def day15-input [512 191])

(defn matches-16 [[n1 n2]]
  (= (bit-and n1 first-16) (bit-and n2 first-16)))

(defn gen-a [n]
  (mod (* n 16807) mersenne))

(defn gen-b [n]
  (mod (* n 48271) mersenne))

(defn gen-a-2 [n]
  (let [res (mod (* n 16807) mersenne)]
    (if (= (mod res 4) 0)
      res
      (recur res))))

(defn gen-b-2 [n]
  (let [res (mod (* n 48271) mersenne)]
    (if (= (mod res 8) 0)
      res
      (recur res))))

(defn day15-p1 [input iterations]
  (->> (zip (iterate gen-a-2 (get input 0))
            (iterate gen-b-2 (get input 1)))
       (drop 1)
       (take iterations)
       (filter matches-16)
       count))


; (p (day15-p1 day15-input iterations1))
; (p (day15-p1 day15-input iterations2))
