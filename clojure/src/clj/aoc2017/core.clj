(ns advent.core
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

; rewrite for part2
(defn base-perm [input]
  {:pos (into [] (range (count input)))
   :symbol (into {} (map (fn [x] [(str x) (str x)]) input))})

(defn spin-p [perm n]
  (update perm :pos #(into [] (rotate-right n %))))

(defn exchange-p [perm a b]
  (-> perm
    (update :pos #(assoc % a (get (:pos perm) b)))
    (update :pos #(assoc % b (get (:pos perm) a)))))

(defn partner-p [perm a b]
  (let [swaps {a b b a}]
    (update perm :symbol #(reduce-kv (fn [m k v] (assoc m k (or (get swaps v) v))) {} %))))

(defn apply-perm [perm]
  (as-> (apply str (map #(get programs %) (:pos perm))) progs
    (apply str (map #(get (:symbol perm) (str %)) progs))))

(defn dance-move [perm input]
  (let [cmd (subs input 0 1)
        args (subs input 1)]
    (cond
      (= cmd "s") (spin-p perm (parse-int args))
      (= cmd "x") (apply exchange-p (concat [perm] (int-vec args)))
      (= cmd "p") (apply partner-p (concat [perm] (string/split args #"/")))
      :else (p "no such inst!" cmd))))

(defn run-dance [perm input]
  (reduce dance-move perm input))

(defn day16-p1 [input]
  (-> (base-perm programs)
      (run-dance input)
      (apply-perm)))

; (p "day16 p1" (t "p1" #(day16-p1 input)))

(defn repeat-perm [perm-result perm]
  {:pos (into [] (map #(get (:pos perm-result) %) (:pos perm)))
   :symbol (reduce-kv (fn [m k v] (assoc m k (get (:symbol perm) v))) {} (:symbol perm-result))})

(defn day16-p2 [input]
  (->> (run-dance (base-perm programs) input)
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (repeat 1000)
       (reduce repeat-perm (base-perm programs))
       (apply-perm)))

; (p "day16 p2" (t "p2" #(day16-p2 input)))

; day 17

(def steps 382)
(def start-buffer {:pos 0 :list [0]})

(defn circle-insert [buf]
  (let [list (:list buf)
        pos (:pos buf)
        next-pos (+ (mod (+ pos steps) (count list)) 1)]
    (if (= 0 (mod (count list) 100000))
      (p (count list)))
    {:pos next-pos
     :list (insert list next-pos (count list))}))

; (p "day17 test" (take 5 (iterate circle-insert start-buffer)))
; (p "day17 part 1" (last (take 2018 (iterate circle-insert start-buffer))))
; (p (last (take 50000001 (iterate circle-insert start-buffer))))
; part 2 hacked together in python

; day 18

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

; (p "day 18 1" (day18-p1 day18-in))

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

; day 23 (builds on day 18)

(def day23-test
  (->> (slurp "input/day23-test.txt")
       (string/split-lines)))

(def day23-in
  (->> (slurp "input/day23.txt")
       (string/split-lines)))

(defn day23-p1 [input]
  (->> (iterate #(execute-p1 input %) [start-state])
       (take 200000)
       (last)))

; (p "day 23 p1" (day23-p1 day23-in))

(defn day23-p1 [input]
  (->> [start-state]
       (map #(assoc-in % [:registers "a"] 1))
       (iterate #(execute-p1 input %))
       (take 400000)
       (last)))

; (p "day 23 p2" (day23-p1 day23-in))

; day 19

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

; day 20

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

; day 21


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

; day 22

(def day22-in (string/split-lines (slurp "input/day22.txt")))
(def day22-test (string/split-lines (slurp "input/day22-test")))

(defn read-virus-grid [input]
  (let [size (count input)
        offset (quot size 2)]
    (->> input
      (map-indexed (fn [y line]
                     (map-indexed (fn [x char]
                                    [[(- x offset) (- size y offset 1)] (str char)])
                                  line)))
      (flatten-once)
      (into {}))))

; (p (read-virus-grid day22-test))

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])
(def state-start {:nodes {} :pos [0 0] :dir 0 :count 0})

(defn get-node [nodes pos]
  (or (get nodes pos) "."))

(defn virus-turn [state]
  (let [node (get-node (:nodes state) (:pos state))
        change (case node
                 "#" 1
                 "W" 0
                 "." (- 1)
                 "F" 2)]
    (assoc state :dir (mod (+ (:dir state) change) 4))))

(defn virus-infect [state]
  (if (= "#" (get-node (:nodes state) (:pos state)))
    (assoc-in state [:nodes (:pos state)] ".")
    (-> state
      (assoc-in [:nodes (:pos state)] "#")
      (update :count inc))))

(defn virus-move [state]
  (let [dir (get dirs (:dir state))]
    (update state :pos (fn [[x y]]
                         [(+ x (get dir 0)) (+ y (get dir 1))]))))

(defn virus-steps [state]
  (-> state
      (virus-turn)
      (virus-infect)
      (virus-move)))

(defn day22-p1 [input]
  (->> state-start
    (#(assoc % :nodes (read-virus-grid input)))
    (iterate virus-steps)
    (drop 1)
    (take 10000)
    (last)))

; (p "day 22 part 1" (:count (day22-p1 day22-in)))

(defn virus-infect2 [state]
  (let [node (get-node (:nodes state) (:pos state))
        path [:nodes (:pos state)]]
    (case node
      "." (assoc-in state path "W")
      "#" (assoc-in state path "F")
      "F" (assoc-in state path ".")
      "W" (-> state
            (assoc-in [:nodes (:pos state)] "#")
            (update :count inc)))))

(defn virus-steps2 [state]
  (-> state
      (virus-turn)
      (virus-infect2)
      (virus-move)))

(defn day22-p2 [input]
  (->> state-start
       (#(assoc % :nodes (read-virus-grid input)))
       (iterate virus-steps2)
       (drop 1)
       (take 10000000)
       (last)))

; 39 s, pretty slow
; (p "day 22 part 2" (:count (day22-p2 day22-in)))

; day 24

; dynamic programming, the strongest bridge that ends with component n at length m
; fail case
; 0/20 20/2 2/2 2/1 1/0
; 0/1 1/2 2/2 2/20 20/100

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

; day 25

; manually converted since I am too lazy to write a parser
(def day25-test
  {:steps 6
   :a {0 [1 1 :b] 1 [0 -1 :b]}
   :b {0 [1 -1 :a] 1 [1 1 :a]}})

(def day25-in
  {:steps 12172063
   :a {0 [1 1 :b] 1 [0 -1 :c]}
   :b {0 [1 -1 :a] 1 [1 -1 :d]}
   :c {0 [1 1 :d] 1 [0 1 :c]}
   :d {0 [0 -1 :b] 1 [0 1 :e]}
   :e {0 [1 1 :c] 1 [1 -1 :f]}
   :f {0 [1 -1 :e] 1 [1 1 :a]}})

(def turing-start
  {:tape {}
   :pos 0
   :state :a})

(defn turing-step [blueprint state]
  (let [curr-val (or (get-in state [:tape (:pos state)])
                     0)
        inst (get-in blueprint [(:state state) curr-val])]
    (-> state
      (assoc-in [:tape (:pos state)] (get inst 0))
      (update :pos + (get inst 1))
      (assoc :state (get inst 2)))))

(defn turing-diag-checksum [state]
  (as-> state s
    (get s :tape)
    (vals s)
    (reduce + s)))

(p (turing-step day25-test turing-start))

(defn day25-p1 [blueprint]
  (as-> (partial turing-step blueprint) t
    (iterate t turing-start)
    (take (inc (:steps blueprint)) t)
    (last t)
    (turing-diag-checksum t)))

(p (day25-p1 day25-test))
(p (day25-p1 day25-in))