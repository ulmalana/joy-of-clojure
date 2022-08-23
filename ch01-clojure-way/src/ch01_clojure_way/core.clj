(ns ch01-clojure-way.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; return lazy seq
(for [x [:a :b], y (range 5) :when (odd? y)]
  [x y])

;; generate side effects (in this case, printing)
(doseq [x [:a :b], y (range 5) :when (odd? y)]
  (prn x y))

(defn r->lfix
  ([a op b]
   (op a b))
  ([a op1 b op2 c]
   (op1 a (op2 b c)))
  ([a op1 b op2 c op3 d]
   (op1 a (op2 b (op3 c d)))))

(defn l->rfix
  ([a op b]
   (op a b))
  ([a op1 b op2 c]
   (op2 c (op1 a b)))
  ([a op1 b op2 c op3 d]
   (op3 d (op2 c (op1 a b)))))

(def order {+ 0, - 0, * 1, / 1})

(defn infix3
  [a op1 b op2 c]
  (if (< (get order op1) (get order op2))
    (r->lfix a op1 b op2 c)
    (l->rfix a op1 b op2 c)))

(def numbers [1 2 3 4 5 6 7 8 9 10])

(apply + numbers)

(defprotocol Concatenable
  (cat2 [this other]))

(extend-type String
  Concatenable
  (cat2 [this other]
    (.concat this other)))

(extend-type java.util.List
  Concatenable
  (cat2 [this other]
    (concat this other)))

;; chess board
(defn initial-board []
  [\r \n \b \q \k \b \n \r
   \p \p \p \p \p \p \p \p
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \- \- \- \- \- \- \- \-
   \P \P \P \P \P \P \P \P
   \R \N \B \Q \K \B \N \R])

(def ^:dynamic *file-key* \a)
(def ^:dynamic *rank-key* \0)

(defn- file-component
  [file]
  (- (int file) (int *file-key*)))

(defn- rank-component
  [rank]
  (->> (int *rank-key*)
       (- (int rank))
       (- 8)
       (* 8)))

(defn- index
  [file rank]
  (+ (file-component file) (rank-component rank)))

(defn lookup
  [board pos]
  (let [[file rank] pos]
    (board (index file rank))))

;; block-level encapsulation
(letfn [(index [file rank]
          (let [f (- (int file) (int \a))
                r (* 8 (- 8 (- (int rank) (int \0))))]
            (+ f r)))]
  (defn lookup2
    [board pos]
    (let [[file rank] pos]
      (board (index file rank)))))

;; local encapsulation
(defn lookup3
  [board pos]
  (let [[file rank] (map int pos)
        [fc rc] (map int [\a \0])
        f (- file fc)
        r (* 8 (- 8 (- rank rc)))
        index (+ f r)]
    (board index)))
