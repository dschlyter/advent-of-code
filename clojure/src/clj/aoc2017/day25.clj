(ns aoc2017.day25
  (:require
    [aoc2017.utils :refer :all]))

; manually converted since I am too lazy to write a parser
(def day25-test
  {:steps 6
   :a {0 [1 1 :b] 1 [0 -1 :b]}
   :b {0 [1 -1 :a] 1 [1 1 :a]}})

(def day25-in
  {:steps 12172063
   :a {0 [1 1 :b] 1 [0 -1 :c]}
   :b {0 [1 -1 :a] 1 [1 -1 :d]}
   :c {0 [1 1 :d] 1 [0 1 :c]}
   :d {0 [0 -1 :b] 1 [0 1 :e]}
   :e {0 [1 1 :c] 1 [1 -1 :f]}
   :f {0 [1 -1 :e] 1 [1 1 :a]}})

(def turing-start
  {:tape {}
   :pos 0
   :state :a})

(defn turing-step [blueprint state]
  (let [curr-val (or (get-in state [:tape (:pos state)])
                     0)
        inst (get-in blueprint [(:state state) curr-val])]
    (-> state
        (assoc-in [:tape (:pos state)] (get inst 0))
        (update :pos + (get inst 1))
        (assoc :state (get inst 2)))))

(defn turing-diag-checksum [state]
  (as-> state s
        (get s :tape)
        (vals s)
        (reduce + s)))

(defn day25-p1 [blueprint]
  (as-> (partial turing-step blueprint) t
        (iterate t turing-start)
        (take (inc (:steps blueprint)) t)
        (last t)
        (turing-diag-checksum t)))

; (p (day25-p1 day25-test))
; (p (day25-p1 day25-in))
