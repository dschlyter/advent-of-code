(ns aoc2017.day8
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day8-in (string/split-lines (slurp "input/day8.txt")))
(def day8-test (string/split-lines (slurp "input/day8-test")))

(def registers (atom {}))
(def register-max (atom (Integer/MIN_VALUE)))

(defn get-reg [name]
  (or (get @registers name) 0))

(defn execute [line]
  (let [elems (string/split line #" ")
        reg (get elems 0)
        op (get elems 1)
        amount (parse-int (get elems 2))
        cmp-target (get elems 4)
        cmp (get elems 5)
        cmp-amount (parse-int (get elems 6))]
    (let [success
          (cond
            (= cmp "==") (= (get-reg cmp-target) cmp-amount)
            (= cmp ">") (> (get-reg cmp-target) cmp-amount)
            (= cmp "<") (< (get-reg cmp-target) cmp-amount)
            (= cmp "<=") (<= (get-reg cmp-target) cmp-amount)
            (= cmp ">=") (>= (get-reg cmp-target) cmp-amount)
            (= cmp "!=") (not= (get-reg cmp-target) cmp-amount)
            :else (print cmp "no such cmp!!!"))]
      (if success
        (cond
          (= op "inc") (swap! registers update reg #(+ (or % 0) amount))
          (= op "dec") (swap! registers update reg #(- (or % 0) amount))
          :else (print op "no such op!!!"))))
    (swap! register-max #(max % (get-reg reg)))))


; (doall (map execute day8-in))
; (p @registers)
; (p "largest:" (apply max (vals @registers)))
; (p "largest-during:" @register-max)
