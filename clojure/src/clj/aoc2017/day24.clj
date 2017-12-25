(ns aoc2017.day24
  (:require
    [aoc2017.utils :refer :all]
    [clojure.string :as string]))

(def day24-in (map int-vec (string/split-lines (slurp "input/day24.txt"))))
(def day24-test (map int-vec (string/split-lines (slurp "input/day24-test.txt"))))

(defn make-connection-map [input]
  (->> input
       (mapcat (fn [[x y]] [[x [x y]] [y [x y]]]))
       (group-by #(get % 0))
       (map-values #(map (fn [[k v]] v) %))))

(def size-matters (atom false))
(def best-bridge (atom {:len 0 :str 0}))

(defn bridge-str [bridge]
  (reduce (fn [s [a b]] (+ s a b)) 0 bridge))

; (p (bridge-str #{[1 2] [2 3]}))

(defn next-port-size [prev-port-size [a b]]
  (if (= prev-port-size a)
    b
    a))

(defn better-bridge [bridge best]
  (if @size-matters
    (or
      (> (:len bridge) (:len best))
      (and
        (>= (:len bridge) (:len best))
        (> (:str bridge) (:str best))))
    (> (:str bridge) (:str best))))

(defn bridge-brute-force [state conns]
  (let [len (count (:comps state))
        str (bridge-str (:comps state))
        bridge-data {:len len :str str}]
    (if (better-bridge bridge-data @best-bridge)
      (do
        (reset! best-bridge bridge-data)
        (println state bridge-data str))))
  (->> (get conns (:next state))
       (remove #(contains? (:comps state) %))
       (map (fn [next-comp] (bridge-brute-force
                              (-> state
                                  (update :comps conj next-comp)
                                  (assoc :next (next-port-size (:next state) next-comp)))
                              conns)))
       (doall)))

(defn day24-p1 [input]
  (let [conns (make-connection-map input)]
    (bridge-brute-force {:comps #{} :next 0} conns)))

(defn day24-p2 [input]
  (reset! size-matters true)
  (day24-p1 day24-in))

; (day24-p1 day24-in)
; (day24-p2 day24-in)
