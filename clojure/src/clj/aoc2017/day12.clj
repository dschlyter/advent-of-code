(ns aoc2017.day12
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day12-test (slurp "input/day12-test"))
(def day12-in (slurp "input/day12.txt"))

(defn build-nodes [input]
  (->> input
       (string/split-lines)
       (map int-vec)
       (reduce #(assoc %1 (first %2) (rest %2)) {})))

(defn dfs [state node]
  (if (not (get (:visited state) node))
    (let [new-state (update state :visited conj node)]
      (reduce dfs new-state (get-in state [:nodes node])))
    state))

(defn start-dfs [input start]
  (dfs {:visited #{} :nodes (build-nodes input)} start))

(defn dfs-all [input]
  (into #{} (map :visited (map #(start-dfs input %) (range 0 (count (string/split-lines input)))))))

; (p (count (:visited (start-dfs day12-in 0))))
; (p (count (dfs-all day12-in)))

