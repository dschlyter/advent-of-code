(ns aoc2017.day10
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

; TODO opt with transients?
(defn knot [rope len]
  (let [pos (:pos rope)
        skip (:skip rope)
        values (rotate pos (:values rope))]
    {
     :values (rotate (- (count values) pos)
                     (concat (reverse (take len (take len values)))
                             (drop len values)))
     :pos    (mod (+ pos len skip) (count values))
     :skip   (inc skip)}))

(def day10-test (int-vec "3,4,1,5"))
(def day10-str "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24")
(def day10-in (int-vec day10-str))

(def knot-start {:values (range 256) :pos 0 :skip 0})

(defn day10-p1 []
  (let [res (:values (reduce knot knot-start day10-in))]
    (* (first res) (nth res 1))))

; (p (day10-p1))

(defn dense-hash [list]
  (map #(apply bit-xor %) (partition 16 list)))

(defn hexify [i]
  (format "%02x" i))

(defn knot-hash [str]
  (->> (map int str)
       (#(concat % [17, 31, 73, 47, 23]))
       (repeat 64)
       flatten
       (reduce knot knot-start)
       :values
       dense-hash
       (map hexify)
       string/join))

; (p (knot-hash ""))
; (p (knot-hash "AoC 2017"))
; (p (knot-hash "1,2,3"))
; (p (knot-hash "1,2,4"))
; (p (knot-hash day10-str))
