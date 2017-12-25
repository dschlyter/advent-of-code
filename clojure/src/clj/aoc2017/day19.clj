(ns aoc2017.day19
  (:require
    [aoc2017.utils :refer :all]
    [clojure.string :as string]))

(def day19-in (string/split-lines (slurp "input/day19.txt")))
(def day19-test (string/split-lines (slurp "input/day19-test.txt")))

(def dirs
  {:down {:add [0 1] :not-first "-" :stop "+" :turn [:right :left]}
   :up {:add [0 (- 1)] :not-first "-" :stop "+" :turn [:right :left]}
   :left {:add [(- 1) 0] :not-first "|" :stop "+" :turn [:up :down]}
   :right {:add [1 0] :not-first "|" :stop "+" :turn [:up :down]}})

(defn get-tube [tubes [x y]]
  (as-> tubes t
        (get t y)
        (if (and (<= 0 x) (< x (count t)))
          (subs t x (inc x))
          " ")))

(def step-count (atom 0))

(defn count-steps []
  (swap! step-count inc))

(defn tube-step [tubes prev-pos dir first]
  (let [pos (into [] (map + prev-pos (:add dir)))
        ch (get-tube tubes pos)]
    ; (p pos ch dir)
    (if (re-matches #"[A-Z]" ch)
      (print ch))
    (if (or (= ch " ")
            (and first (= ch (:not-first dir))))
      pos ; done!
      (do
        (count-steps)
        (if (= ch (:stop dir))
          (doall (map #(tube-step tubes pos (get dirs %) true) (:turn dir)))
          (recur tubes pos dir false))))))

; (tube-step day19-test [5 0] (:down dirs) true)
; (tube-step day19-in [113 0] (:down dirs) true)
; (println)
; (println (+ 1 @step-count))
