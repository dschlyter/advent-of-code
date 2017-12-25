(ns aoc2017.day22
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day22-in (string/split-lines (slurp "input/day22.txt")))
(def day22-test (string/split-lines (slurp "input/day22-test")))

(defn read-virus-grid [input]
  (let [size (count input)
        offset (quot size 2)]
    (->> input
         (map-indexed (fn [y line]
                        (map-indexed (fn [x char]
                                       [[(- x offset) (- size y offset 1)] (str char)])
                                     line)))
         (flatten-once)
         (into {}))))

; (p (read-virus-grid day22-test))

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])
(def state-start {:nodes {} :pos [0 0] :dir 0 :count 0})

(defn get-node [nodes pos]
  (or (get nodes pos) "."))

(defn virus-turn [state]
  (let [node (get-node (:nodes state) (:pos state))
        change (case node
                 "#" 1
                 "W" 0
                 "." (- 1)
                 "F" 2)]
    (assoc state :dir (mod (+ (:dir state) change) 4))))

(defn virus-infect [state]
  (if (= "#" (get-node (:nodes state) (:pos state)))
    (assoc-in state [:nodes (:pos state)] ".")
    (-> state
        (assoc-in [:nodes (:pos state)] "#")
        (update :count inc))))

(defn virus-move [state]
  (let [dir (get dirs (:dir state))]
    (update state :pos (fn [[x y]]
                         [(+ x (get dir 0)) (+ y (get dir 1))]))))

(defn virus-steps [state]
  (-> state
      (virus-turn)
      (virus-infect)
      (virus-move)))

(defn day22-p1 [input]
  (->> state-start
       (#(assoc % :nodes (read-virus-grid input)))
       (iterate virus-steps)
       (drop 1)
       (take 10000)
       (last)))

; (p "day 22 part 1" (:count (day22-p1 day22-in)))

(defn virus-infect2 [state]
  (let [node (get-node (:nodes state) (:pos state))
        path [:nodes (:pos state)]]
    (case node
      "." (assoc-in state path "W")
      "#" (assoc-in state path "F")
      "F" (assoc-in state path ".")
      "W" (-> state
              (assoc-in [:nodes (:pos state)] "#")
              (update :count inc)))))

(defn virus-steps2 [state]
  (-> state
      (virus-turn)
      (virus-infect2)
      (virus-move)))

(defn day22-p2 [input]
  (->> state-start
       (#(assoc % :nodes (read-virus-grid input)))
       (iterate virus-steps2)
       (drop 1)
       (take 10000000)
       (last)))

; 39 s, pretty slow
; (p "day 22 part 2" (:count (day22-p2 day22-in)))
