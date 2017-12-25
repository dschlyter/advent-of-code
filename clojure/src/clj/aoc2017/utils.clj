(ns aoc2017.utils
  (:require [clojure.string :as string]))

(defn tap [s fn]
  (fn s)
  s)

(defn p [& args]
  (apply println args)
  (first args))

(defn t [name f]
  (let [start (System/nanoTime)
        ret (f)]
    (p "Time" name (/ (- (System/nanoTime) start) 1e9))
    ret))

(defn parse-int
  ([s] (Integer/parseInt s))
  ([s b] (Integer/parseInt s b)))

(defn pad-zero [s n]
  (apply str (take-last n (concat (repeat n \0) s))))

(defn neg-range [a b]
  (reverse
    (range (inc b) (inc a))))

(defn insert [vec pos value]
  (into []
        (concat (subvec vec 0 (min pos (count vec)))
                [value]
                (subvec vec (min pos (count vec))))))

(defn flatten-once [coll]
  (mapcat identity coll))

(defn map-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn zip [seq1 seq2]
  (map vector seq1 seq2))

(defn cardinal []
  (iterate inc 1))

(defn indexed [sequence]
  (zip (cardinal) sequence))

(defn int-vec [str]
  (->> str
       (#(string/split % #"[^-0-9]+"))
       (filter #(not= % ""))
       (map parse-int)
       (into [])))

(defn long-vec [str]
  (->> str
       (#(string/split % #"[^-0-9]+"))
       (filter #(not= % ""))
       (map #(Long/parseLong %))
       (into [])))

(defn rotate [n list]
  (concat (drop n list) (take n list)))

(defn rotate-right [n list]
  (rotate (- (count list) n) list))
