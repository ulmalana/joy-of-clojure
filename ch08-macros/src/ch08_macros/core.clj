(ns ch08-macros.core
  (:require [clojure.walk :as walk])
  (:use [clojure.xml :as xml])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net URL])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; the result of the previous expression is passed
;; as the first argument of the outermost function
;; in the next expression
(-> (/ 144 12) ; 12
    (/ 2 3)    ; (/ 12 2 3)
    str        ; (str 2)
    keyword    ; (keyword "2")
    list)      ; (list :2)

(-> (/ 144 12)     ; 12
    (* 4 (/ 2 3))  ; (* 12 4 (/ 2 3))
    str            ; (str 32)
    keyword        ; (keyword "32")
    (list :33))    ; (list :32 :33)

(eval 56)
(eval '(list 1 2))
(eval (list (symbol "+") 1 2))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(contextual-eval '{a 1, b 2} '(+ a b))

(contextual-eval '{a 1, b 2} '(let [b 1000] (+ a b)))

(let [x 9, y '(- x)]
  (println `y)
  (println ``y)
  (println ``~y)
  (contextual-eval {'x 36} ``~~y))

;;;;;;;;; control structure ;;;;;;;;;;;;;;;;;;;;;
(defmacro do-until [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(macroexpand-1 '(do-until true (prn 1) false (prn 2)))

(walk/macroexpand-all '(do-until true (prn 1) false (prn 2)))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless true (println "nope"))
(unless false (println "yay"))

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

;; (def-watched x 2) will expand to
;;
;;(do (def x 2)
;;    (add-watch (var x)
;;               :re-bind
;;               (fn [key r old new]
;;                 (println old " -> " new))))

(def-watched x (* 12 12))
(def x 0)


;;;;;;;;;;;;;;; domain modeling ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro domain [name & body]
  `{:tag :domain,
    :attrs {:name (str '~name)},
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
    :attrs {:name (str '~name)},
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
            (list? a) [:isa (str (second a))]
            (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties, :attrs nil,
     :content (apply vector (for [p props]
                              {:tag :property,
                               :attrs {:name (str (first p))},
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "This is a man"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce yet elusive creature"
                     [eats-goats?]))))

(:tag d)
(:tag (first (:content d)))

;;(xml/emit d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro resolution [] `x)
(macroexpand '(resolution))

(def x 9)
;; => #'ch08-macros.core/x
(let [x 109] (resolution))
;; => 9

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(awhen [1 2 3] (it 2))
;; => 3

(awhen nil (println "Will never get here"))
;; => nil

(awhen 1 (awhen 2 [it]))
;; => [2]

(defn joc-www []
  (-> "http://www.joyofclojure.com/hello" URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

;;(let [stream (joc-www)]
;;  (with-open [page stream]
;;    (println (.readLine page))
;;    (print "The stream will close now..."))
;;  (println "but lets read from it anyway.")
;;  (.readLine stream))

(defmacro with-resource [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

;;(let [stream (joc-www)]
;;  (with-resource [page stream]
;;    #(.close %)
;;    (.readLine page)))

;;;;;;;;;;;;;;;;;; contract macro ;;;;;;;;;;;;;;;;;;;;;;

(declare collect-bodies)
(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)
(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) 'require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) 'ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception.
                                  (str "Unknown tag "
                                       (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))
            [x y]
            (require
             (pos? x)
             (pos? y))
            (ensure
             (= (* 2 (+ x y)) %))))

(def times2 (partial doubler-contract #(* 2 %)))

(times2 10)

(def times3 (partial doubler-contract #(* 3 %)))

;;(times3 9)

((partial doubler-contract #(* 2 (+ %1 %2))) 2 3)
