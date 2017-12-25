(ns aoc2017.day7
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day7-in (string/split-lines (slurp "input/day7")))
(def day7-test (string/split-lines (slurp "input/day7-test")))

(defn get-targets [line]
  (if-let [sec (get (string/split line #"->") 1)]
    (into [] (map string/trim (string/split sec #",")))))

(defn get-source [line]
  (if-let [fst (get (string/split line #"->") 0)]
    (get (string/split fst #" ") 0)))

(defn get-weight [line]
  (if-let [fst (get (string/split line #"->") 0)]
    (parse-int (get (string/split fst #"[()]") 1))))

(defn build-tower [lines]
  (->> lines
       (map (fn [line] {(get-source line) {:weight (get-weight line) :targets (get-targets line)}}))
       (reduce merge)))

(defn children-of [tower k]
  (get-in tower [k :targets]))

(defn child-nodes [tower k]
  (select-keys tower (children-of tower k)))

(defn get-root [tower]
  (let [children (into #{} (flatten (map :targets (vals tower))))]
    (first
      (filter #(not (contains? children %)) (keys tower)))))

(defn sum-weights [tower key]
  (let [tower-w-child-weight (reduce (fn [a k] (sum-weights a k))
                                     tower
                                     (children-of tower key))
        child-sum (reduce + (map #(get-in tower-w-child-weight [% :sum])
                                 (children-of tower key)))]
    (assoc-in tower-w-child-weight [key :sum] (+ (get-in tower [key :weight]) child-sum))))

(defn heaviest-program [nodes]
  (last (sort-by #(get (val %) :sum) nodes)))

(defn walk-heavy [tower k]
  (p ">>>" k (get tower k) ">>>" (child-nodes tower k))
  (let [big (heaviest-program (child-nodes tower k))]
    (if big
      (walk-heavy tower (key big))
      nil)))

; hack with some manual inspection, abuse knowledge of the deviant node being heavier
(defn day7-p2 []
  (let [tower (build-tower day7-in)
        tower2 (sum-weights tower (get-root tower))]
    (walk-heavy tower2 (get-root tower2))))

; (p (get-root (build-tower day7-in)))
; (p (day7-p2))
