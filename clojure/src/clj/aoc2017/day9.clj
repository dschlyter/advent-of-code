(ns aoc2017.day9
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(defn score [s]
  (:sum
    (reduce #(let [cs (:score %1) sum (:sum %1)]
               (cond
                 (= %2 \{) {:score (inc cs) :sum (+ sum cs)}
                 (= %2 \}) {:score (dec cs) :sum sum}
                 (= %2 \,) %1
                 :else (do (println "cannot score" %2) %1)))
            {:score 1 :sum 0}
            s)))

(def stream-char-count (atom 0))

(defn clean [s]
  (-> s
      (string/replace #"!." "")
      (tap #(reset! stream-char-count (count %1)))
      (string/replace #"<[^>]*>" "<>")
      (tap #(println (- @stream-char-count (count %)) "characters cleaned"))
      (string/replace #"<>" "")))

(def clean-score (comp score clean))

(def day9-in (slurp "input/day9.txt"))

; (p (clean-score "{{<!!>},{<!!>},{<!!>},{<!!>}}"))
; (p (clean-score "{{<a!>},{<a!>},{<a!>},{<ab>}}"))
; (p (clean-score day9-in))
