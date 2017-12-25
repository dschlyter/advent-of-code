(ns aoc2017.day11
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

; model as a 2d array with the possibility to go diagonally on even/uneven

(defn point
  ([pair] {:x (get pair 0) :y (get pair 1)})
  ([p1 p2] {:x p1 :y p2}))

(defn add [pos change]
  {:x (+ (:x pos) (:x change)) :y (+ (:y pos) (:y change))})

(defn move [pos dir]
  (let [dirs {
              "n" [[0 1] [0 1]]
              "s" [[0 -1] [0 -1]]
              "ne" [[1 0] [1 1]]
              "se" [[1 -1] [1 0]]
              "nw" [[-1 0] [-1 1]]
              "sw" [[-1 -1] [-1 0]]}]
    (add pos (point (-> dirs
                        (get dir)
                        (get (if (even? (:x pos)) 0 1)))))))


(defn moves [pos moves-str]
  (let [move-seq (string/split (string/trim moves-str) #",")]
    (reduce move pos move-seq)))

(defn all-moves [pos moves-str]
  (let [move-seq (string/split (string/trim moves-str) #",")]
    (reductions move pos move-seq)))

(defn dist [p1 p2]
  (let [xdist (Math/abs (- (:x p1) (:x p2)))
        ydist (Math/abs (- (:y p1) (:y p2)))
        base-dist (+ xdist (max 0 (- ydist (quot xdist 2))))]
    (if (not= xdist 1)
      base-dist
      (if (even? (:x p1))
        (if (> (:y p1) (:y p2))
          ydist
          base-dist)
        (if (< (:y p1) (:y p2))
          ydist
          base-dist)))))

(defn moves-dist [pos moves-str]
  (dist pos (moves pos moves-str)))

; (p (moves-dist (point 0 0) "ne,ne,ne"))
; (p (moves-dist (point 0 0) "ne,ne,sw,sw"))
; (p (moves-dist (point 0 0) "ne,ne,s,s"))
; (p (moves (point 0 0) "se,sw,se,sw,sw"))
; (p (moves-dist (point 0 0) "se,sw,se,sw,sw"))

(def day11-in (slurp "input/day11.txt"))

; (p (moves-dist (point 0 0) day11-in))

; (p (apply max (map #(dist (point 0 0) %))))
; (all-moves (point 0 0) day11-in)
