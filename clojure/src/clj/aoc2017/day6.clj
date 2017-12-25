(ns aoc2017.day6
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day6-p1-test-input [0 2 7 0])

(def day6-p1-input "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11")

(defn max-item-index [sequence]
  (get
    (reduce-kv (fn [[bi bv] i v]
                 (if (> v bv) [i v] [bi bv]))
               [-1 -1]
               sequence)
    0))

(defn redist [registers index element-count]
  (cond
    (<= element-count 0) registers
    :else (recur (update registers index inc)
                 (mod (inc index) (count registers))
                 (dec element-count))))

(defn realloc-cycle [registers]
  (let [max-index (max-item-index registers)]
    (redist (assoc registers max-index 0)
            (mod (inc max-index) (count registers))
            (get registers max-index))))

(defn register-seq [registers]
  (iterate realloc-cycle registers))

(defn state-set-seq [registers]
  (drop 2
        (reductions conj #{} (register-seq registers))))

(defn repeat-count [registers]
  (first
    (first
      (drop-while (fn [[i s]] (= (inc i) (count s)))
                  (indexed (state-set-seq registers))))))

; not very opt
(defn day6-p2 [registers]
  (let [rep-count (repeat-count registers)
        repeating-register (first (drop rep-count (register-seq registers)))]
    (inc
      (count (take-while #(not= repeating-register %)
                         (drop (inc rep-count) (register-seq registers)))))))

; (p (repeat-count (int-vec day6-p1-input)))
; (p (day6-p2 (int-vec day6-p1-input)))
