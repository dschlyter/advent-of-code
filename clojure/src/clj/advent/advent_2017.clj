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

(defn parse-int [s]
      (Integer/parseInt s))

(defn neg-range [a b]
  (reverse
    (range (inc b) (inc a))))

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

; TODO read from resource file
(def d1input "68376334795224855827459835293967497295464175589881588256882344699473595413912688278647235862566123233983921662578792917453912795352746426512649965615919588512125567186837411371179875287621488759761429629174886972298349197722423458299323141529413191327622485249495864168181327197661454464926326248274999448373741839963155646828842752761293142356422964355349521987483211496361289666375779728345952231649453711684539164893151811849653331845998998597991146881361717234517911759893792348815818755262456378627116779495435596139617246571678531183335956244163871445674244765586446362529159854137535962117184875192273872222899887357292312978286182636232921252574738118347521187637829623831872437381979223955675634257889137823684924127338433248519515211796732599314921611399736571277222546332369461136277417419794865524123989722492356536832313937597437717873787593849468836733642529378547151146397532997237439387663769334722979172954835154486382983716698212694357398153392926255272961384626131829678171219569288685597141132355322788254163923888378155573948753185423158997877718687642446457446643422536541238979761725496426292359382168535641216124211741896552562128941824172241913873537828976172738276983915232241451589421911121567228899853934667954786256223614621554618294467191255153395256524786159758429643756586457639177183891162214163549688595416893383194995824534247841414247526268212761954913719452114876764745799982792594753759626334319631191917894368116738893548797661111899664138398354818931135486984944719992393148681724116616741428937687985152658296679845474766477741553632712968679175356452987459761126437216758171182395219393289199148996813762849991484678429793578629331215796996751484375784895561682156658579887518746862371751372692472765217374791324656745291574784495299477362964676351148183676897122366838656342745944945275263617729359831466565694983217252594237828187612857523344265418227883219383138893873384775659548637662867572687198263688597865118173921615178165442133987362382721444844952715592955744739873677838847693982379696776")

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
       (#(:values (reduce knot knot-start %)))
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
