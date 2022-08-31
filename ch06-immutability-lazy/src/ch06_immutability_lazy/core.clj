(ns ch06-immutability-lazy.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; structural sharing
(def baselist (list :barnabas :adam))
(def list1 (cons :willie baselist))
(def list2 (cons :phoenix baselist))


(= (next list1) (next list2))
;;=> true (ie. they are equal)

(identical? (next list1) (next list2))
;;=> true (but also the same objects)

;; conj for tree
(defn xconj [t v]
  (cond
    (nil? t) {:val v :L nil :R nil}
    (< v (:val t)) {:val (:val t),
                    :L (xconj (:L t) v),
                    :R (:R t)}
    :else {:val (:val t),
           :L (:L t),
           :R (xconj (:R t) v)}))

(def tree1 (xconj nil 5))
(def tree1 (xconj tree1 3))
(def tree1 (xconj tree1 2))

;; get the value of the tree
(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(def tree2 (xconj tree1 7))
(identical? (:L tree1) (:L tree2))

;;;;;;;;;;;;;; laziness ;;;;;;;;;;;;;;;;;;;;;;;;;
;; for any false, this will short circuit
(defn if-chain [x y z]
  (if x
    (if y
      (if z
        (do
          (println "made it!")
          :all-truthy)))))

;; shorter version of if-chain using and function
(defn and-chain [x y z]
  (and x y z (do (println "Made it!") :all-truthy)))

;; create a deep nested structure with steps function
;;(steps [1 2 3 4]) => [1 [2 [3 [4 []]]]]
;; not using can cause stack overflow
(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    []))

;; rest vs next
(def very-lazy
  (-> (iterate #(do (print \.) (inc %)) 1)
      rest rest rest))

(def less-lazy
  (-> (iterate #(do (print \.) (inc %)) 1)
      next next next))

;; rec-step with lazy sequence
(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [])))

(lz-rec-step [1 2 3 4])

(dorun (lz-rec-step (range 100000))) ;; wont overflow

(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

;; infinite sequences
(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(map triangle (range 1 11))

(def tri-nums (map triangle (iterate inc 1)))

(take 10 tri-nums)
(take 10 (filter even? tri-nums))

;; delay and force evaluation
(defn defer-expensive [cheap expensive]
  (if-let [good-enough (force cheap)]
    good-enough
    (force expensive)))

(defer-expensive (delay :cheap)
  (delay (do (Thread/sleep 5000) :expensive)))

;;(defer-expensive (delay false)
;;  (delay (do (Thread/sleep 5000) :expensive)))

(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l] (:head l))
(defn tail [l] (force (:tail l)))
(def tri-nums (inf-triangles 1))

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))

(taker 10 tri-nums)
(nthr tri-nums 99)

;;;;;;;;; quicksort ;;;;;;;;;;;;;;;;;
(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list*
                 (filter smaller? xs)
                 pivot
                 (remove smaller? xs)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))
