(ns aoc2017.day14
  (:require
    [aoc2017.utils :refer :all]
    [aoc2017.day10 :refer [knot-hash]]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day14-input "oundnydw")
(def day14-test "flqrgnkx")

(defn to-bin [i]
  (pad-zero (Integer/toString i 2) 4))

(defn to-bin-str [hexstr]
  (apply str (map #(to-bin (parse-int (str %) 16)) hexstr)))

(defn hashes [input]
  (map #(knot-hash (str input "-" %)) (range 128)))

(defn count-1 [strings]
  (->> strings
       (apply str)
       (filter #(= \1 %))
       count))

(defn day14-p1 [input]
  (->> input
       hashes
       (map to-bin-str)
       count-1))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn dfs2 [graph start visited pos]
  (let [node (get-in graph pos)]
    (if (and (= node \1) (not (get visited pos)))
      (reduce #(dfs2 graph start %1 %2)
              (assoc visited pos start)
              (neighbours pos))
      visited)))

(defn all-positions [graph]
  (flatten-once (map (fn [y] (map (fn [x] [y x]) (range (count (get graph y)))))
                     (range (count graph)))))

(defn dfs2-all [graph]
  (reduce #(dfs2 graph %2 %1 %2) {} (all-positions graph)))

(defn regions [graph]
  (into #{} (vals (dfs2-all graph))))

(defn day14-p2 [input]
  (->> input
       hashes
       (map to-bin-str)
       (into [])
       regions
       count))

; (p (day14-p1 day14-input))
; (p (day14-p2 day14-input))
