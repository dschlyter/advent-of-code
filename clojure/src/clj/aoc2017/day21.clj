(ns aoc2017.day21
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day21-in (string/split-lines (slurp "input/day21.txt")))
(def day21-test (string/split-lines (slurp "input/day21-test")))
(def start-pattern ".#./..#/###")

; how to rotate and flip strings on the format ../.#
; 2 is /
(def perm-2 {:rotate [3 0 2
                      4 1]
             :flip [3 4 2
                    0 1]})
; 3,7 is /
(def perm-3 {:rotate [8 4 0 3
                      9 5 1 7
                      10 6 2]
             :flip [8 9 10 3
                    4 5 6 7
                    0 1 2]})

(defn apply-perm [str perm]
  (string/join (map #(get str %) perm)))

(defn all-perms [str perms]
  (->> (take 4 (iterate #(apply-perm % (:rotate perms)) str))
       (map (fn [key] [key (apply-perm key (:flip perms))]))
       flatten))

(defn all-keys [key]
  (cond
    (= (count key) 5) (all-perms key perm-2)
    (= (count key) 11) (all-perms key perm-3)
    :else (println "key len missmatch :/")))

(defn make-rules [input]
  (->> input
       (map (fn [line] (let [parts (string/split line #" ")]
                         [(get parts 0) (get parts 2)])))
       (map (fn [[k v]] (for [key (all-keys k) val [v]]
                          [key val])))
       flatten-once
       (into {})))

; (p (doall (all-keys "12/45")))
; (p (doall (all-keys "123/456/789")))
; (p (make-rules day21-test))

(defn grid-join [size pieces]
  (->> pieces
       (map #(string/split % #"/"))
       (partition size size [])
       (map #(apply map vector %))
       flatten-once
       (map #(string/join %))
       (string/join "/")))

(defn grid-split [grid size]
  (->> grid
       (#(string/split % #"/"))
       (map #(partition size %))
       (partition size)
       (map #(apply map vector %))
       (flatten-once)
       (map #(map string/join %))
       (map #(string/join "/" %))))

; (p (grid-join ["12/34" "12/34" "33/44" "33/44"] 2))
; (p (grid-split "1212/3434/3333/4444" 2))

(defn enhance [grid rules]
  (let [split-size (if (= (mod (count (string/replace grid #"/" "")) 2) 0) 2 3)
        join-size (/ (count (first (string/split grid #"/"))) split-size)]
    (->>
      (grid-split grid split-size)
      (map #(get rules %))
      (grid-join join-size))))

(defn count-on [grid]
  (count (string/replace grid #"[^#]" "")))

(defn print-grid [grid]
  (p (string/replace grid "/" "\n"))
  (p "----"))

(defn day21-p1 [input]
  (let [rules (make-rules input)]
    (->> start-pattern
         (iterate #(enhance % rules))
         (map #(tap % print-grid))
         (take 6)
         last
         count-on)))

(defn day21-p2 [input]
  (let [rules (make-rules input)]
    (->> start-pattern
         (iterate #(enhance % rules))
         (take 19)
         last
         count-on)))

; (p (day21-p1 day21-in))
; (p (day21-p2 day21-in))
