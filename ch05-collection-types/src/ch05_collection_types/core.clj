(ns ch05-collection-types.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;; not persistent
(def ds (into-array [:willie :barnabas :adam]))
(seq ds)

;; try to modify
(aset ds 1 :quentin)
(seq ds)

;; persistent
(def ds [:wille :barnabas :adam])

(def ds1 (replace {:barnabas :quentin} ds))

ds

ds1

;;;;;;;;;;;;;;;;;; vectors ;;;;;;;;;;;;;;;;;;;;;;;;;
;; build vector
(def vect [1 2 3])

(vec (range 10))

(vector 1 2 3)

(let [my-vector [:a :b :c]]
  (into my-vector (range 10)))

;; set the primitive type of vector
(into (vector-of :int) [Math/PI 2 1.2])
(into (vector-of :char) [100 101 102])

;; large vectors
(def a-to-j (vec (map char (range 65 75))))
a-to-j

(seq a-to-j)
(rseq a-to-j)

;; modify
(assoc a-to-j 4 "no longer E")
(replace {2 :a, 4 :b} [1 2 3 2 3 4])

(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(get-in matrix [1 2])
(assoc-in matrix [1 2] 'x)
(update-in matrix [1 2] * 100)

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

;; vectors as stacks
(def my-stack [1 2 3])

(peek my-stack)
;; => 3

(pop my-stack)
;; => [1 2]

(conj my-stack 4)
;; => [1 2 3 4]

(+ (peek my-stack) (peek (pop my-stack)))
;; => 5

;; replacing reverse with vector
;; using list (need to reverse)
(defn strict-map1 [f coll]
  (loop [coll coll, acc nil]
    (if (empty? coll)
      (reverse acc) ;; reverse the result
      (recur (next coll)
             (cons (f (first coll)) acc)))))

(strict-map1 - (range 5))
;; using vector (no reverse)
(defn strict-map2 [f coll]
  (loop [coll coll, acc []] ;; use vector
    (if (empty? coll)
      acc
      (recur (next coll)
             (conj acc (f (first coll)))))))

(strict-map2 - (range 5))

;;; subvector
(subvec a-to-j 3 6)

;; vector as map entries
(doseq [[dimension amount] {:width 10, :height 20, :depth 15}]
  (println (str (name dimension) ":") amount "inches"))
