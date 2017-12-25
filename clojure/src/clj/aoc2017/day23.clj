(ns aoc2017.day23
  (:require
    [aoc2017.utils :refer :all]
    [aoc2017.day18 :refer :all]
    [clojure.string :as string]))

(def day23-test
  (->> (slurp "input/day23-test.txt")
       (string/split-lines)))

(def day23-in
  (->> (slurp "input/day23.txt")
       (string/split-lines)))

(defn day23-p1 [input]
  (->> (iterate #(execute-p1 input %) [start-state])
       (take 200000)
       (last)))

; (p "day 23 p1" (day23-p1 day23-in))

(defn day23-p1 [input]
  (->> [start-state]
       (map #(assoc-in % [:registers "a"] 1))
       (iterate #(execute-p1 input %))
       (take 400000)
       (last)))

; (p "day 23 p2" (day23-p1 day23-in))
