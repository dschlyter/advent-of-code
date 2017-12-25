(ns aoc2017.day16
  (:require
    [aoc2017.utils :refer :all]
    [clojure.string :as string]))

(def day16-test "s1,x3/4,pe/b")
(def input-test (-> day16-test (string/trim) (string/split #",")))
(def day16-in (slurp "input/day16.txt"))
(def input (-> day16-in (string/trim) (string/split #",")))

(def programs "abcdefghijklmnop")
; (def programs "abcde")

; rewrite for part2
(defn base-perm [input]
  {:pos (into [] (range (count input)))
   :symbol (into {} (map (fn [x] [(str x) (str x)]) input))})

(defn spin-p [perm n]
  (update perm :pos #(into [] (rotate-right n %))))

(defn exchange-p [perm a b]
  (-> perm
      (update :pos #(assoc % a (get (:pos perm) b)))
      (update :pos #(assoc % b (get (:pos perm) a)))))

(defn partner-p [perm a b]
  (let [swaps {a b b a}]
    (update perm :symbol #(reduce-kv (fn [m k v] (assoc m k (or (get swaps v) v))) {} %))))

(defn apply-perm [perm]
  (as-> (apply str (map #(get programs %) (:pos perm))) progs
        (apply str (map #(get (:symbol perm) (str %)) progs))))

(defn dance-move [perm input]
  (let [cmd (subs input 0 1)
        args (subs input 1)]
    (cond
      (= cmd "s") (spin-p perm (parse-int args))
      (= cmd "x") (apply exchange-p (concat [perm] (int-vec args)))
      (= cmd "p") (apply partner-p (concat [perm] (string/split args #"/")))
      :else (p "no such inst!" cmd))))

(defn run-dance [perm input]
  (reduce dance-move perm input))

(defn day16-p1 [input]
  (-> (base-perm programs)
      (run-dance input)
      (apply-perm)))

; (p "day16 p1" (t "p1" #(day16-p1 input)))

(defn repeat-perm [perm-result perm]
  {:pos (into [] (map #(get (:pos perm-result) %) (:pos perm)))
   :symbol (reduce-kv (fn [m k v] (assoc m k (get (:symbol perm) v))) {} (:symbol perm-result))})

(defn day16-p2 [input]
  (->> (run-dance (base-perm programs) input)
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (apply-perm)))

; (p "day16 p2" (t "p2" #(day16-p2 input)))
