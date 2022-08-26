(ns ch04-scalars.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;; truncation ;;;;;;;;;;;;;;;;;
;; not truncated with M
(let [imadeuapi 3.14159265358979323846264338327950288419716939937M]
  (println (class imadeuapi))
  imadeuapi)

;; truncated by default
(let [butieatedit 3.14159265358979323846264338327950288419716939937]
  (println (class butieatedit))
  butieatedit)

;;;;;;;;;;;;;;;;;;;; promotion ;;;;;;;;;;;;;;;;;;;;;;;
(def clueless 9)

(class clueless) ;; java.lang.Long

(class (+ clueless 9000000000000000000)) ;; still java.lang.Long

(class (+ clueless 900000000000000000000000)) ;; promoted to clojure.lang.BigInt

(class (+ clueless 9.0)) ;; java.lang.Double

;;;;;;;;;;;;;;;;; overflow ;;;;;;;;;;;;;;;;;;;;;;;;;

;; instead of overflow the number (wrapping around) and propagating
;; inaccuracies, clojure will throw an exception.
;(+ Long/MAX_VALUE Long/MAX_VALUE)

;; use unchecked function like unchecked-add to perform operation that
;; may overflow.
(unchecked-add (Long/MAX_VALUE) (Long/MAX_VALUE))

;;;;;;;;;;;;;;;;;;;; rounding error ;;;;;;;;;;;;;;;;;;;;;;;;;
(let [approx-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours (* 3600 100 10)
      actual-total (double (* hours actual-interval))
      approx-total (double (* hours approx-interval))]
  (- actual-total approx-total))

;;;;;;;;;;;;;;;;;;;; rational ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rationals are the best choice for absolute preciese calculations,
;; but they are slower than floating points and double.

;; how to rationalize
(def a (rationalize 1.0e50))
(def b (rationalize -1.0e50))
(def c (rationalize 17.0e00))

;; both of these have the same results.
;; try to remove rationalize function above and see the difference.
(+ (+ a b) c)
(+ a (+ b c))

;; extract the component of rational number
(numerator (/ 123 10))
(denominator (/ 123 10))

;;;;;;;;;;;;;;;;;;;;;; keywords ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def population {:zombies 2700 :humans 9})

;; as keys
(println (/ (get population :zombies)
            (get population :humans))
         "zombies per capita")

;; as functions
(println (/ (:zombies population)
            (:humans population))
         "zombies per capita")

;; as directives
(defn pour [lb ub]
  (cond
    (= ub :toujours) (iterate inc lb)
    :else (range lb ub)))


;;;;;;;;;;;;;;;;;;;;; symbolic resolution ;;;;;;;;;;;;;;;;;;;;;;

;; two symbols with the same name may not be the same objects
;; since both symbols may have different metadata
(identical? 'goat 'goat) ;; false
(= 'goat 'goat) ;; true

(let [x 'goat, y x]
  (identical? x y)) ;; true


;; metadata
(let [x (with-meta 'goat {:ornery true})
      y (with-meta 'goat {:ornery false})]
  [(= x y) ;; true
   (identical? x y) ;; false
   (meta x) ;; {:ornery true}
   (meta y) ;; {:ornery false}
   ])

;;;;;;;;;;;;;;;;;;;;;; regex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(re-seq #"\w+" "one-two/three") ;; ("one" "two" "three")
(re-seq #"\w*(\w)" "one-two/three") ;; (["one" "e"] ["two" "o"] ["three" "e"])
