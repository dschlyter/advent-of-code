(ns aoc2017.day17
  (:require
    [aoc2017.utils :refer :all]))

(def steps 382)
(def start-buffer {:pos 0 :list [0]})

(defn circle-insert [buf]
  (let [list (:list buf)
        pos (:pos buf)
        next-pos (+ (mod (+ pos steps) (count list)) 1)]
    (if (= 0 (mod (count list) 100000))
      (p (count list)))
    {:pos next-pos
     :list (insert list next-pos (count list))}))

; (p "day17 test" (take 5 (iterate circle-insert start-buffer)))
; (p "day17 part 1" (last (take 2018 (iterate circle-insert start-buffer))))
; (p (last (take 50000001 (iterate circle-insert start-buffer))))

; part 2 hacked together in python

