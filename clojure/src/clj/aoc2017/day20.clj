(ns aoc2017.day20
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day20-in
  (->> (slurp "input/day20.txt")
       (string/split-lines)
       (map long-vec)
       (map (fn [x] [(subvec x 0 3) (subvec x 3 6) (subvec x 6 9)]))
       (map #(zip [:p :v :a] %))
       (map #(into {} %))
       (map-indexed (fn [i v] (assoc v :i i)))))

(defn manhattan-dist-acc [particle]
  (->> (:a particle)
       (map #(Math/abs %))
       (reduce +)))

(defn day20-p1 [input]
  (sort-by manhattan-dist-acc input))

; (p "day 20 part 1" (take 2 (day20-p1 day20-in)))

(defn add3 [pos vec]
  [(+ (get pos 0) (get vec 0))
   (+ (get pos 1) (get vec 1))
   (+ (get pos 2) (get vec 2))])

(defn update-particle [particle]
  (let [velocity (add3 (:v particle) (:a particle))
        pos (add3 (:p particle) velocity)]
    (-> particle
        (assoc :p pos)
        (assoc :v velocity))))

(defn find-duplicates [numbers]
  (->> numbers
       (frequencies)
       (filter (fn [[k v]] (> v 1)))
       (keys)
       (into #{})))

(defn remove-duplicates [particles]
  (let [dups (->> particles
                  (map :p)
                  (find-duplicates))]
    (remove #(contains? dups (:p %)) particles)))

(defn particle-step [particles]
  (->> particles
       (map update-particle)
       (remove-duplicates)))

(defn day20-p2 [input]
  (->> input
       (iterate particle-step)
       (take 2000)
       (last)
       (count)))

; (p "day 20 p 2" (day20-p2 day20-in))
