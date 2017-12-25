(ns aoc2017.day18
  (:require
    [aoc2017.utils :refer :all]
    [clojure.java.io :refer [as-file make-parents]]
    [clojure.string :as string]
    [clojure.pprint :as pp :refer [pprint]]))

(def day18-test
  (->> (slurp "input/day18-test")
       (string/split-lines)))

(def day18-test2
  (->> (slurp "input/day18-test2.txt")
       (string/split-lines)))

(def day18-in
  (->> (slurp "input/day18.txt")
       (string/split-lines)))

(def start-state {:line 0 :registers {} :mul-count 0 :send nil :send-count 0 :recv []})

(defn get-val [registers token]
  (if (re-matches #"-?[0-9]+" token)
    (parse-int token)
    (or (get registers token) 0)))

; updated with ops from day 23
(defn execute [lines state]
  (let [line (string/split (or (get lines (:line state) "end"))
                           #" ")
        regs (:registers state)
        op (first line)
        arg1 (nth line 1 nil)
        arg2 (nth line 2 nil)
        next (update state :line inc)]
    ; (p "exec" op arg1 arg2 @registers)
    (case op
      "end" (assoc state :end true)
      "nop" next
      "snd" (-> next
                (assoc :send (get-val regs arg1))
                (update :send-count inc))
      "set" (assoc-in next [:registers arg1] (get-val regs arg2))
      "add" (assoc-in next [:registers arg1] (+ (get-val regs arg1) (get-val regs arg2)))
      "sub" (assoc-in next [:registers arg1] (- (get-val regs arg1) (get-val regs arg2)))
      "mul" (-> next
                (update :mul-count inc)
                (assoc-in [:registers arg1] (* (get-val regs arg1) (get-val regs arg2))))
      "mod" (assoc-in next [:registers arg1] (mod (get-val regs arg1) (get-val regs arg2)))
      "rcv" (if-let [recv (first (:recv state))]
              (-> next
                  ; (p (get-val regs arg1)) ; print for part 1
                  (assoc-in [:registers arg1] recv)
                  (update :recv #(drop 1 %)))
              state) ; state is unchanged
      "jgz" (if (> (get-val regs arg1) 0)
              (assoc state :line (+ (:line state) (get-val regs arg2)))
              next)
      "jnz" (if (not= (get-val regs arg1) 0)
              (assoc state :line (+ (:line state) (get-val regs arg2)))
              next)
      :else (print op "no such op!!!"))))

(defn transfer-one [states from to]
  (if-let [send (get-in states [from :send])]
    (-> states
        (update-in [to :recv] #(concat % [send]))
        (assoc-in [from :send] nil))
    states))

(defn transfer [states]
  (if (= (count states) 2)
    (-> states
        (transfer-one 0 1)
        (transfer-one 1 0))
    (transfer-one states 0 0)))

; make part 1 conform to part 2 model
(defn only-last-recv [state]
  (if (< 1 (count (:recv state)))
    (update state :recv #(list (last %)))
    state))

(defn execute-p1 [input states]
  (->> states
       (map #(execute input %))
       (map only-last-recv)
       (into [])
       (transfer)))

(defn day18-p1 [input]
  (->> (iterate #(execute-p1 input %) [start-state])
       (take 2000)
       (last)))

(p "day 18 1" (day18-p1 day18-in))

(defn get-op [lines index]
  (p "getop" lines index)
  (let [line (string/split (get lines index) #" ")]
    (first line)))

(defn is-deadlock [lines states]
  (not-any? #(or (not= "rcv" (get-op lines (:line %)))
                 (> (count (:recv %)) 0))
            states))

(defn execute-all [input states]
  (->> states
       (map #(execute input %))
       (into [])
       (transfer)))

(defn day18-p2 [input]
  (->> [start-state start-state]
       (map-indexed #(assoc-in %2 [:registers "p"] %1))
       (iterate #(execute-all input %))
       (take 100000)
       (last)))

; (p "day 18 2" (day18-p2 day18-in))
