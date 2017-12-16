(ns advent.advent-2017
    (:require
      [clojure.java.io :refer [as-file make-parents]]
      [clojure.string :as string]
      [clojure.pprint :as pp :refer [pprint]]
      [com.walmartlabs.datascope :as ds]))

(println "loading advent 2017" (.toString (java.time.LocalDateTime/now)))

; utils

(defn tap [s fn]
  (fn s)
  s)

(defn p [& args]
  (apply println args)
  (first args))

(defn parse-int
  ([s] (Integer/parseInt s))
  ([s b] (Integer/parseInt s b)))

(defn pad-zero [s n]
  (apply str (take-last n (concat (repeat n \0) s))))

(defn neg-range [a b]
  (reverse
    (range (inc b) (inc a))))

(defn flatten-once [coll]
  (mapcat identity coll))

(defn zip [seq1 seq2]
  (map vector seq1 seq2))

(defn cardinal []
  (iterate inc 1))

(defn indexed [sequence]
  (zip (cardinal) sequence))

(defn int-vec [str]
  (->> str
       (#(string/split % #"[^0-9]+"))
       (map parse-int)
       (into [])))

(p "")

; day 1

(def d1input (string/trim (slurp "input/day1.txt")))

(defn day1-p1 [input]
      (->> input
           (#(string/split % #""))
           (map parse-int)
           (#(cons (last %) %))
           (partition 2 1)
           (filter #(apply = %)) (map first) (reduce +)))

(defn day1-p2 [input]
      (->> input
           (#(string/split % #""))
           (map parse-int)
           (#(let [half (/ (count %) 2)]
               (map vector % (concat (drop half %) (take half %)))))
           (filter #(apply = %)) (map first) (reduce +)))

; (p (day1-p1 d1input))
; (p (day1-p2 d1input))

; day 3 part 1
; the nth ring contains (n/2+1)^2 entries minus all previous entries
; it is on the 302:th ring. It contains 2408 entries (603^2 - 601^2), each segment 602 entries, 301 entries in each half.
; Entry is 2082 entries from the end (603^2 - input), or 326 entries from the start, or 25 entries north of the middle.
; So 301 steps right and 25 steps north? - yep!

; day 3 part 2

; note: zero-indexed
(defn gen-spiral-path [n]
  (if (= n 0)
    {:x 0 :y 0}
    (concat
      (for [y (range (inc (- n)) (inc n))]
        {:x n :y y})
      (for [x (neg-range (dec n) (dec (- n)))]
        {:x x :y n})
      (for [y (neg-range (dec n) (dec (- n)))]
        {:x (- n) :y y})
      (for [x (range (inc (- n)) (inc n))]
        {:x x :y (- n)}))))

(defn spiral-path [n]
  (lazy-seq (concat (gen-spiral-path n) (spiral-path (inc n)))))

(defn squares-near [point]
  (for [x (range (dec (:x point)) (+ (:x point) 2))
        y (range (dec (:y point)) (+ (:y point) 2))]
       {:x x :y y}))

(defn spiral-value [point spiral]
  (->> (squares-near point)
       (map #(or (get spiral %) 0))
       (reduce +)))

(defn spiral-write [point spiral]
  (let [value (spiral-value point spiral)]
    {:last-value value :spiral (assoc spiral point value)}))

(defn spiral-values [spiral path]
  (let [write (spiral-write (first path) spiral)]
    (lazy-seq (cons
                (:last-value write)
                (spiral-values (:spiral write) (drop 1 path))))))

(defn day3-p2 []
  (first
    (drop-while #(< % 361527)
                (spiral-values {{:x 0 :y 0} 1} (spiral-path 1)))))

; day 6

(def day6-p1-test-input [0 2 7 0])

(def day6-p1-input "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11")

(defn max-item-index [sequence]
  (get
    (reduce-kv (fn [[bi bv] i v]
                 (if (> v bv) [i v] [bi bv]))
               [-1 -1]
               sequence)
    0))

(defn redist [registers index element-count]
  (cond
    (<= element-count 0) registers
    :else (recur (update registers index inc)
                 (mod (inc index) (count registers))
                 (dec element-count))))

(defn realloc-cycle [registers]
  (let [max-index (max-item-index registers)]
    (redist (assoc registers max-index 0)
            (mod (inc max-index) (count registers))
            (get registers max-index))))

(defn register-seq [registers]
  (iterate realloc-cycle registers))

(defn state-set-seq [registers]
  (drop 2
        (reductions conj #{} (register-seq registers))))

(defn repeat-count [registers]
  (first
    (first
      (drop-while (fn [[i s]] (= (inc i) (count s)))
                  (indexed (state-set-seq registers))))))

; not very opt
(defn day6-p2 [registers]
  (let [rep-count (repeat-count registers)
        repeating-register (first (drop rep-count (register-seq registers)))]
    (inc
      (count (take-while #(not= repeating-register %)
                         (drop (inc rep-count) (register-seq registers)))))))

; (p (repeat-count (int-vec day6-p1-input)))
; (p (day6-p2 (int-vec day6-p1-input)))

; day 7


(def day7-in (string/split-lines (slurp "input/day7")))
(def day7-test (string/split-lines (slurp "input/day7-test")))

(defn get-targets [line]
  (if-let [sec (get (string/split line #"->") 1)]
    (into [] (map string/trim (string/split sec #",")))))

(defn get-source [line]
  (if-let [fst (get (string/split line #"->") 0)]
    (get (string/split fst #" ") 0)))

(defn get-weight [line]
  (if-let [fst (get (string/split line #"->") 0)]
    (parse-int (get (string/split fst #"[()]") 1))))

(defn build-tower [lines]
  (->> lines
       (map (fn [line] {(get-source line) {:weight (get-weight line) :targets (get-targets line)}}))
       (reduce merge)))

(defn children-of [tower k]
  (get-in tower [k :targets]))

(defn child-nodes [tower k]
  (select-keys tower (children-of tower k)))

(defn get-root [tower]
  (let [children (into #{} (flatten (map :targets (vals tower))))]
    (first
      (filter #(not (contains? children %)) (keys tower)))))

(defn sum-weights [tower key]
  (let [tower-w-child-weight (reduce (fn [a k] (sum-weights a k))
                                     tower
                                     (children-of tower key))
        child-sum (reduce + (map #(get-in tower-w-child-weight [% :sum])
                                 (children-of tower key)))]
    (assoc-in tower-w-child-weight [key :sum] (+ (get-in tower [key :weight]) child-sum))))

(defn heaviest-program [nodes]
  (last (sort-by #(get (val %) :sum) nodes)))

(defn walk-heavy [tower k]
  (p ">>>" k (get tower k) ">>>" (child-nodes tower k))
  (let [big (heaviest-program (child-nodes tower k))]
    (if big
      (walk-heavy tower (key big))
      nil)))

; hack with some manual inspection, abuse knowledge of the deviant node being heavier
(defn day7-p2 []
  (let [tower (build-tower day7-in)
        tower2 (sum-weights tower (get-root tower))]
    (walk-heavy tower2 (get-root tower2))))

; (p (get-root (build-tower day7-in)))
; (p (day7-p2))

(def day8-in (string/split-lines (slurp "input/day8.txt")))
(def day8-test (string/split-lines (slurp "input/day8-test")))

(def registers (atom {}))
(def register-max (atom (Integer/MIN_VALUE)))

(defn get-reg [name]
  (or (get @registers name) 0))

(defn execute [line]
  (let [elems (string/split line #" ")
        reg (get elems 0)
        op (get elems 1)
        amount (parse-int (get elems 2))
        cmp-target (get elems 4)
        cmp (get elems 5)
        cmp-amount (parse-int (get elems 6))]
    (let [success
          (cond
            (= cmp "==") (= (get-reg cmp-target) cmp-amount)
            (= cmp ">") (> (get-reg cmp-target) cmp-amount)
            (= cmp "<") (< (get-reg cmp-target) cmp-amount)
            (= cmp "<=") (<= (get-reg cmp-target) cmp-amount)
            (= cmp ">=") (>= (get-reg cmp-target) cmp-amount)
            (= cmp "!=") (not= (get-reg cmp-target) cmp-amount)
            :else (print cmp "no such cmp!!!"))]
      (if success
        (cond
          (= op "inc") (swap! registers update reg #(+ (or % 0) amount))
          (= op "dec") (swap! registers update reg #(- (or % 0) amount))
          :else (print op "no such op!!!"))))
    (swap! register-max #(max % (get-reg reg)))))


; (doall (map execute day8-in))
; (p @registers)
; (p "largest:" (apply max (vals @registers)))
; (p "largest-during:" @register-max)

; day 9

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

; day 10

(defn rotate [n list]
  (concat (drop n list) (take n list)))

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

; day 11

; model as a 2d array with the possibility to go diagonally on even/uneven

(defn point
  ([pair] {:x (get pair 0) :y (get pair 1)})
  ([p1 p2] {:x p1 :y p2}))

(defn add [pos change]
  {:x (+ (:x pos) (:x change)) :y (+ (:y pos) (:y change))})

(defn move [pos dir]
  (let [dirs {
              "n" [[0 1] [0 1]]
              "s" [[0 -1] [0 -1]]
              "ne" [[1 0] [1 1]]
              "se" [[1 -1] [1 0]]
              "nw" [[-1 0] [-1 1]]
              "sw" [[-1 -1] [-1 0]]}]
    (add pos (point (-> dirs
                        (get dir)
                        (get (if (even? (:x pos)) 0 1)))))))


(defn moves [pos moves-str]
  (let [move-seq (string/split (string/trim moves-str) #",")]
    (reduce move pos move-seq)))

(defn all-moves [pos moves-str]
  (let [move-seq (string/split (string/trim moves-str) #",")]
    (reductions move pos move-seq)))

(defn dist [p1 p2]
  (let [xdist (Math/abs (- (:x p1) (:x p2)))
        ydist (Math/abs (- (:y p1) (:y p2)))
        base-dist (+ xdist (max 0 (- ydist (quot xdist 2))))]
    (if (not= xdist 1)
      base-dist
      (if (even? (:x p1))
        (if (> (:y p1) (:y p2))
          ydist
          base-dist)
        (if (< (:y p1) (:y p2))
          ydist
          base-dist)))))

(defn moves-dist [pos moves-str]
  (dist pos (moves pos moves-str)))

; (p (moves-dist (vec 0 0) "ne,ne,ne"))
; (p (moves-dist (vec 0 0) "ne,ne,sw,sw"))
; (p (moves-dist (vec 0 0) "ne,ne,s,s"))
; (p (moves (vec 0 0) "se,sw,se,sw,sw"))
; (p (moves-dist (vec 0 0) "se,sw,se,sw,sw"))

(def day11-in (slurp "input/day11.txt"))

; (p (moves-dist (vec 0 0) day11-in))

; (p (apply max (map #(dist (vec 0 0) %)
                   ; (all-moves (vec 0 0) day11-in))))

; day 12

(def day12-test (slurp "input/day12-test"))
(def day12-in (slurp "input/day12.txt"))

(defn build-nodes [input]
  (->> input
       (string/split-lines)
       (map int-vec)
       (reduce #(assoc %1 (first %2) (rest %2)) {})))

(defn dfs [state node]
  (if (not (get (:visited state) node))
      (let [new-state (update state :visited conj node)]
        (reduce dfs new-state (get-in state [:nodes node])))
      state))

(defn start-dfs [input start]
  (dfs {:visited #{} :nodes (build-nodes input)} start))

(defn dfs-all [input]
  (into #{} (map :visited (map #(start-dfs input %) (range 0 (count (string/split-lines input)))))))

; (p (count (:visited (start-dfs day12-in 0))))
; (p (count (dfs-all day12-in)))

; day 13

(def day13-test (slurp "input/day13-test"))
(def day13-in (slurp "input/day13.txt"))

(defn day13-p1 [input]
  (->> input
       string/trim
       string/split-lines
       (map int-vec)
       (filter (fn [[d r]] (= 0 (mod d (+ r (max 0 (- r 2)))))))
       (map #(apply * %))
       (reduce +)))

(defn build-periods [input]
  (->> input
       string/trim
       string/split-lines
       (map int-vec)
       (map #(update % 1 (fn [n] (+ n (max 0 (- n 2))))))))

(defn is-caught [wait periods]
  (some (fn [[d p]] (= 0 (mod (+ d wait) p))) periods))

; (p (day13-p1 day13-test))
; (let [periods (build-periods day13-in)]
  ; (p (filter #(not (is-caught % periods)) (range 10000000))))

; day 14

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

; day 15

(def mersenne 2147483647)
(def iterations1 40000000)
(def iterations2 5000000)
; (def iterations 5)

(def first-16 (dec (bit-shift-left 1 16)))

(def day15-test [65 8921])
(def day15-input [512 191])

(defn matches-16 [[n1 n2]]
  (= (bit-and n1 first-16) (bit-and n2 first-16)))

(defn gen-a [n]
  (mod (* n 16807) mersenne))

(defn gen-b [n]
  (mod (* n 48271) mersenne))

(defn gen-a-2 [n]
  (let [res (mod (* n 16807) mersenne)]
    (if (= (mod res 4) 0)
      res
      (recur res))))

(defn gen-b-2 [n]
  (let [res (mod (* n 48271) mersenne)]
    (if (= (mod res 8) 0)
      res
      (recur res))))

(defn day15-p1 [input iterations]
  (->> (zip (iterate gen-a-2 (get input 0))
            (iterate gen-b-2 (get input 1)))
       (drop 1)
       (take iterations)
       (filter matches-16)
       count))


; (p (day15-p1 day15-input iterations1))
; (p (day15-p1 day15-input iterations2))

; day 16

(def day16-test "s1,x3/4,pe/b")
(def input-test (-> day16-test (string/trim) (string/split #",")))
(def day16-in (slurp "input/day16.txt"))
(def input (-> day16-in (string/trim) (string/split #",")))

(def programs "abcdefghijklmnop")
; (def programs "abcde")

(defn rotate-right [n list]
  (rotate (- (count list) n) list))

(defn spin [programs n]
  (apply str (rotate-right n programs)))

(defn partner [programs a b]
  (-> programs
      (string/replace (re-pattern a) "A")
      (string/replace (re-pattern b) "B")
      (string/replace #"A" b)
      (string/replace #"B" a)))

(defn exchange [programs a b]
  (partner programs (str (get programs a))
                    (str (get programs b))))

(defn dance-move [programs input]
  (let [cmd (subs input 0 1)
        args (subs input 1)]
    (cond
      (= cmd "s") (spin programs (parse-int args))
      (= cmd "x") (apply exchange (concat [programs] (int-vec args)))
      (= cmd "p") (apply partner (concat [programs] (string/split args #"/")))
      :else (p "no such inst!" cmd))))

(defn run-dance [programs input]
  (reduce dance-move programs input))

; part 1
(defn day16-p1 [input]
  (run-dance programs input))

(p "day16 p1" (day16-p1 input))
