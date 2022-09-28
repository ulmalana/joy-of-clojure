(ns ch15-performance.core
  (:require [criterium.core :as crit]
            [clojure.core.reducers :as r])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;; (defn asum-sq
;;   [xs]
;;   (let [db1 (amap xs i ret
;;                   (* (aget xs i)
;;                      (aget xs i)))]
;;     (areduce db1 i ret 0
;;              (+ ret (aget db1 i)))))

;; somehow this is not working
;; (time (dotimes [_ 10000] (asum-sq (float-array [1 2 3 4 5]))))
;; => 
;; => 

;; with type hints
(defn ^Double asum-sq-hint ;; type hint for return value
  [^floats xs]            ;; type hint for argument
  (let [^floats db1 (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce db1 i ret 0
             (+ ret (aget db1 i)))))
;; but this works
(time (dotimes [_ 10000] (asum-sq-hint (float-array [1 2 3 4 5]))))
;; => nil
;; "Elapsed time: 8.301677 msecs"

(defn zencat1
  [x y]
  (loop [src y, ret x]
    (if (seq src)
      (recur (next src) (conj ret (first src)))
      ret)))

(zencat1 [1 2 3] [4 5 6])
;; => [1 2 3 4 5 6]

(time (dotimes [_ 1000000] (zencat1 [1 2 3] [4 5 6])))
;; "Elapsed time: 574.961377 msecs"
;; => nil

;; with transient
(defn zencat2
  [x y]
  (loop [src y, ret (transient x)]
    (if src
      (recur (next src) (conj! ret (first src)))
      (persistent! ret))))
(zencat2 [1 2 3] [4 5 6])
;; => [1 2 3 4 5 6]

(time (dotimes [_ 1000000] (zencat2 [1 2 3] [4 5 6])))
;; "Elapsed time: 707.826297 msecs"
;; => nil

(def bv (vec (range 1e6)))

(first (time (zencat1 bv bv)))
;; "Elapsed time: 252.69064 msecs"
;; => 0

(first (time (zencat2 bv bv)))
;; "Elapsed time: 97.848968 msecs"
;; => 0

;;;;;;;;;; chunked seqs ;;;;;;;;;;;;;

(def gimme #(do (print \.) %))
(take 1 (map gimme (range 32)))
;; ................................(0)

(take 1 (map gimme (range 33)))
;; ................................(0)

(take 1 (drop 32 (map gimme (range 64))))
;; ................................................................(32)

(defn seq1 [s]
  (lazy-seq
   (when-let [[x] (seq s)]
     (cons x (seq1 (rest s))))))

(take 1 (map gimme (seq1 (range 32))))
;; .(0)

(take 1 (drop 32 (map gimme (seq1 (range 64)))))
;; .................................(32)

;;;;;;;;;;;;;;; memoization ;;;;;;;;;;;;;;;;;;;;

(def gcd (memoize
          (fn [x y]
            (cond
              (> x y) (recur (- x y) y)
              (< x y) (recur x (- y x))
              :else x))))

(gcd 1000645475 56130776629010010);; => 215

(defprotocol CacheProtocol
  (lookup [cache e])
  (has? [cache e])
  (hit [cache e])
  (miss [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result))))

(def cache (BasicCache. {}))

(lookup (miss cache '(servo) :robot) '(servo))
;; => :robot

(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item)))

(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]
    (with-meta
      (fn [& args]
        (let [cs (swap! cache through (.f cache-impl) args)]
          @(lookup cs args)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))
(def sometimes-slowly (memoization-impl
                       (PluggableMemoization. slowly (BasicCache. {}))))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; "Elapsed time: 3007.771051 msecs"
;; => [108 108]

(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; "Elapsed time: 0.189359 msecs"
;; => [108 108]

;;;;;;;;;;;;;; coercion ;;;;;;;;;;;;;;;;;;;

(defn factorial-a [original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))
;; => #'ch15-performance.core/factorial-a

(factorial-a 10)
;; => 3628800

(factorial-a 20)
;; => 2432902008176640000

(time (dotimes [_ 1e5] (factorial-a 20)))
;; "Elapsed time: 308.89271 msecs"
;; => nil

(time (dotimes [_ 1e5] (factorial-a 20)))
;; "Elapsed time: 297.941802 msecs"
;; => nil

(defn factorial-b [original-x]
  (loop [x (long original-x), acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))
;; => #'ch15-performance.core/factorial-b

(time (dotimes [_ 1e5] (factorial-b 20)))
;; "Elapsed time: 37.422773 msecs"
;; => nil

(defn factorial-c [^long original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))
;; => #'ch15-performance.core/factorial-c

(time (dotimes [_ 1e5] (factorial-c 20)))
;; "Elapsed time: 35.102544 msecs"
;; => nil

;; giving up accuracy for performance
(defn factorial-e [^double original-x]
  (loop [x original-x, acc 1.0]
    (if (>= 1.0 x)
      acc
      (recur (dec x) (* x acc)))))
;; => #'ch15-performance.core/factorial-e

(factorial-e 20.0)
;; => 2.43290200817664E18

(factorial-e 30.0)
;; => 2.652528598121911E32

(factorial-e 171.0)
;; => ##Inf

(time (dotimes [_ 1e5] (factorial-e 20.0)))
;; "Elapsed time: 24.400508 msecs"
;; => nil

;;; giving up performance for accuracy with auto-promotion
(defn factorial-f [^long original-x]
  (loop [x original-x, acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (*' x acc)))))
;; => #'ch15-performance.core/factorial-f

(factorial-f 20)
;; => 2432902008176640000

(factorial-f 30)
;; => 265252859812191058636308480000000N

(factorial-f 171)
;; => 1241018070217667823424840524103103992616605577501693185388951803611996075221691752992751978120487585576464959501670387052809889858690710767331242032218484364310473577889968548278290754541561964852153468318044293239598173696899657235903947616152278558180061176365108428800000000000000000000000000000000000000000N

(time (dotimes [_ 1e5] (factorial-f 20)))
;; "Elapsed time: 152.274062 msecs"
;; => nil

;;;;;;;;;;;; reducibles ;;;;;;;;;;;;;;;;;;

(defn empty-range? [start end step]
  (or (and (pos? step) (>= start end))
      (and (neg? step) (<= start end))))

(defn lazy-range [i end step]
  (lazy-seq
   (if (empty-range? i end step)
     nil
     (cons i
           (lazy-range (+ i step)
                       end
                       step)))))

(lazy-range 5 10 2)
;; => (5 7 9)

(lazy-range 6 0 -1)
;; => (6 5 4 3 2 1)

(reduce conj [] (lazy-range 6 0 -1))
;; => [6 5 4 3 2 1]

(reduce + 0 (lazy-range 6 0 -1))
;; => 21

(defn reducible-range [start end step]
  (fn [reducing-fn init]
    (loop [result init, i start]
      (if (empty-range? i end step)
        result
        (recur (reducing-fn result i) (+ i step))))))
;; => #'ch15-performance.core/reducible-range

(def countdown-reducible (reducible-range 6 0 -1))
;; => #'ch15-performance.core/countdown-reducible

(countdown-reducible conj [])
;; => [6 5 4 3 2 1]

(countdown-reducible + 0)
;; => 21

(defn half [x]
  (/ x 2))
;; => #'ch15-performance.core/half

(defn sum-half [result input]
  (+ result (half input)))
;; => #'ch15-performance.core/sum-half

(reduce sum-half 0 (lazy-range 0 10 2))
;; => 10

((reducible-range 0 10 2) sum-half 0)
;; => 10

(defn half-transformer [f1]
  (fn f1-half [result input]
    (f1 result (half input))))

((reducible-range 0 10 2) (half-transformer +) 0)
;; => 10

((reducible-range 0 10 2) (half-transformer conj) [])
;; => [0 1 2 3 4]

(defn mapping [map-fn]
  (fn map-transformer [f1]
    (fn [result input]
      (f1 result (map-fn input)))))

((reducible-range 0 10 2) ((mapping half) +) 0)
;; => 10

((reducible-range 0 10 2) ((mapping half) conj) [])
;; => [0 1 2 3 4]

((reducible-range 0 10 2) ((mapping list) conj) [])
;; => [(0) (2) (4) (6) (8)]

(defn filtering [filter-pred]
  (fn [f1]
    (fn [result input]
      (if (filter-pred input)
        (f1 result input)
        result))))

((reducible-range 0 10 2) ((filtering #(not= % 2)) +) 0)
;; => 18

((reducible-range 0 10 2) ((filtering #(not= % 2)) conj) [])
;; => [0 4 6 8]

(defn mapcatting [map-fn]
  (fn [f1]
    (fn [result input]
      (let [reducible (map-fn input)]
        (reducible f1 result)))))

(defn and-plus-ten [x]
  (reducible-range x (+ 11 x) 10))

((and-plus-ten 5) conj [])
;; => [5 15]

((reducible-range 0 10 2) ((mapcatting and-plus-ten) conj) [])
;; => [0 10 2 12 4 14 6 16 8 18]

(defn r-map [mapping-fn reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((mapping mapping-fn) reducing-fn) init)))

(defn r-filter [filter-pred reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((filtering filter-pred) reducing-fn) init)))

(def our-final-reducible
  (r-filter #(not= % 2)
            (r-map half (reducible-range 0 10 2))))

(our-final-reducible conj [])
;; => [0 1 3 4]
;; => 

(crit/bench
 (reduce + 0 (filter even? (map half (lazy-range 0 (* 10 1000 1000) 2)))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;; Execution time mean : 3.495271 sec
;; Execution time std-deviation : 42.906178 ms
;; Execution time lower quantile : 3.435615 sec ( 2.5%)
;; Execution time upper quantile : 3.591172 sec (97.5%)
;; Overhead used : 11.939340 ns

;; Found 4 outliers in 60 samples (6.6667 %)
;; low-severe	 3 (5.0000 %)
;; low-mild	 1 (1.6667 %)
;; Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
;; nil

(crit/bench
 (reduce + 0 (filter even? (map half (range 0 (* 10 1000 1000) 2)))))
;; Evaluation count : 60 in 60 samples of 1 calls.
;; Execution time mean : 1.287359 sec
;; Execution time std-deviation : 11.006365 ms
;; Execution time lower quantile : 1.273513 sec ( 2.5%)
;; Execution time upper quantile : 1.311751 sec (97.5%)
;; Overhead used : 11.939340 ns

;; Found 5 outliers in 60 samples (8.3333 %)
;; low-severe	 4 (6.6667 %)
;; low-mild	 1 (1.6667 %)
;; Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
;; nil

(crit/bench ((r-filter even? (r-map half (reducible-range 0 (* 10 1000 1000) 2))) + 0))
;; Evaluation count : 60 in 60 samples of 1 calls.
;; Execution time mean : 1.601251 sec
;; Execution time std-deviation : 13.980156 ms
;; Execution time lower quantile : 1.578495 sec ( 2.5%)
;; Execution time upper quantile : 1.625100 sec (97.5%)
;; Overhead used : 11.939340 ns

;; Found 1 outliers in 60 samples (1.6667 %)
;; low-severe	 1 (1.6667 %)
;; Variance from outliers : 1.6389 % Variance is slightly inflated by outliers
;; nil


(defn core-r-map [mapping-fn core-reducible]
  (r/reducer core-reducible (mapping mapping-fn)))

(defn core-r-filter [filter-pred core-reducible]
  (r/reducer core-reducible (filtering filter-pred)))

(defn reduce-range [reducing-fn init, start end step]
  (loop [result init, i start]
    (if (empty-range? i end step)
      result
      (recur (reducing-fn result i) (+ i step)))))

(defn core-reducible-range [start end step]
  (reify protos/CollReduce
    (coll-reduce [this reducing-fn init]
      (reduce-range reducing-fn init, start end step))
    (coll-reduce [this reducing-fn]
      (if (empty-range? start end step)
        (reducing-fn)
        (reduce-range reducing-fn start, (+ start step) end step)))))
