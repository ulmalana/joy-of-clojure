(ns ch02-quick-tour.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;; scalars ;;;;;;;;;;;;;;;;;;

;; integers
34
-45
+432
989476675476586748784573895723873042398735974584
[127 0x7f 0177 32r3v 2r01111111] ;; all are 127

;; floating points
23.1
+56.34343
-2.454
34e7
989e-8
10.7e-6

;; rationals
22/7
-7/22
847584795576/298

;; symbols
(def yucky-pi 22/7)

;; keywords
:hehe
:thiskeyword
:43
:?

;; strings
"This is string"
"Also a
String"

;; characters
\a
\A
\u0042 ;; \B
\\
\u30DE ;; katakana "ma"

;;;;;;;;;;;;; collections ;;;;;;;;;;;;;;;;;;;

;; lists
'(7 8 9 0)
()
(:haha :wkwkw "lol")
'(4 5 (j l m) 9 6)

;; vectors
[1 2 :t :o :? "hehe"]

;; maps
{1 "this" 2 "is" 3 "a" 4 "map"}
{:name "tanjiro" :age 17}

;; sets
#{1 2 3 4 5}
#{"string" :key \r 123}

;;;;;;;;;;;;;;;;; functions ;;;;;;;;;;;;;;;;;;

;; anonymous functions
(fn [x y]
  (println "Making a set")
  #{x y})

;; passing args to anonymous functions
((fn [x y]
  (println "Making a set")
   #{x y})
 7 8)

;; function with def
(def make-set
  (fn [x y]
    (println "with def")
    #{x y}))

;; function with defn
(defn make-set'
  "create function with defn"
  [x y]
  (println "making a set")
  #{x y})

;; multiple arities
(defn make-set''
  ([x] #{x})
  ([x y] #{x y}))

;; variable arity
(defn arity2+ [first second & more]
  (vector first second more))

;; in-place function
(def make-list
  #(list %1 %2 %3))

;;;;;;;;;;;;;; locals, loops, blocks ;;;;;;;;;;;;;;

;; blocks
(do
  (def x 5)
  (def y 9)
  (+ x y)
  [x y])

;; locals
(let [r 5
      pi 3.1415
      r-squared (* r r)]
  (println "radius is " r)
  (* pi r-squared))

;; recur
(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(defn sum-down-from [sum x]
  (if (pos? x)
    (recur (+ sum x) (dec x))
    sum))


;; loop
(defn sum-down-from' [initial-x]
  (loop [sum 0, x initial-x]
    (if (pos? x)
      (recur (+ sum x) (dec x))
      sum)))

;; tail position
(defn absolute-val [x]
  (if (pos? x)
    x
    (- x)))

;;;;;;;;;;;;;;;; preventing evaluation ;;;;;;;;;;;;;;;;;;

;; quote
(cons 1 (quote (2 3)))
(quote (cons 1 (2 3)))
(cons 1 '(2 3))

;; syntax quote
`(1 2 3)
`(map)
`(map even? [1 2 3]) ;; (clojure.core/map clojure.core/even? [1 2 3])

;; unquote
`(+ 10 (* 3 2)) ;; (clojure.core/+ 10 (clojure.core/* 3 2))
`(+ 10 ~(* 3 2)) ;; (clojure.core/+ 10 6)

(let [x 2]
  `(1 ~x 3)) ;; (1 2 3)

(let [x '(2 3)]
  `(1 ~x)) ;; (1 (2 3))

;; unquote splicing
(let [x '(2 3)]
  `(1 ~@x)) ;; (1 2 3)

;; auto gensym
`this-is-gensym#

;;;;;;;;;;;;;;;;;;;;;;;; interop ;;;;;;;;;;;;;;;;;;;;;;;

;; accesing static class members
java.util.Locale/JAPAN
(Math/sqrt 9) ;; calling java.lang.Math#sqrt

;; creating instances
(new java.awt.Point 0 1)
(java.awt.Point. 45 78)

(new java.util.HashMap {"haha" 545 "bar" 90})
(java.util.HashMap. {"haha" 545 "bar" 90})


;; accessing instance variables
(.-x (java.awt.Point. 35 33)) ;; 35
(.-y (java.awt.Point. 35 33)) ;; 33

;; accessing instance methods
(.divide (java.math.BigDecimal. "42") 2M)

;; setting instance fields
(let [origin (java.awt.Point. 0 0)]
  (set! (.-x origin) 12)
  (str origin))

;; .. macro (similar to -> or ->>)
(.endsWith (.toString (java.util.Date.)) "2022")
(.. (java.util.Date.) toString (endsWith "2022"))

;; doto macro
(doto (java.util.HashMap.)
  (.put "HOME" "/home/me")
  (.put "SRC" "src")
  (.put "BIN" "classes"))

;;;;;;;;;;;;;;; exceptions ;;;;;;;;;;;;;;;;;;;;;;;;;

;; throw and catch

; (throw (Exception. "This is exception @@@@"))

(defn throw-catch [func]
  [(try
     (func)
     (catch ArithmeticException e "No dividing by zero!")
     (catch Exception e (str "You are so bad " (.getMessage e)))
     (finally (println "returning... ")))])
