(ns ch17-last-ch.core
  (:require [clojure.set :as ra])
  (:use [clojure.string :as str :only []])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def artists
  #{{:artist "Burial" :genre-id 1}
    {:artist "Magma" :genre-id 2}
    {:artist "Can" :genre-id 3}
    {:artist "Faust" :genre-id 3}
    {:artist "Ikonika" :genre-id 1}
    {:artist "Grouper"}})

(def genres
  #{{:genre-id 1 :genre-name "Dubstep"}
    {:genre-id 2 :genre-name "Zeuhl"}
    {:genre-id 3 :genre-name "Prog"}
    {:genre-id 4 :genre-name "Drone"}})

(def ALL identity)

(ra/select ALL genres)
;; => #{{:genre-id 4, :genre-name "Drone"} {:genre-id 2, :genre-name "Zeuhl"} {:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(ra/select (fn [m] (#{1 3} (:genre-id m))) genres)
;; => #{{:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(defn ids [& ids]
  (fn [m] ((set ids) (:genre-id m))))

(ra/select (ids 1 3) genres)
;; => #{{:genre-id 3, :genre-name "Prog"} {:genre-id 1, :genre-name "Dubstep"}}

(take 2 (ra/select ALL (ra/join artists genres)))
;; => ({:genre-id 2, :genre-name "Zeuhl", :artist "Magma"} {:genre-id 1, :genre-name "Dubstep", :artist "Ikonika"})

(defn shuffle-expr [expr]
  (if (coll? expr)
    (if (= (first expr) `unquote)
      "?"
      (let [[op & args] expr]
        (str "("
             (str/join (str " " op " ")
                       (map shuffle-expr args)) ")")))
    expr))

(shuffle-expr 42)
;; => 42

(shuffle-expr `(unquote max))
;; => "?"

(read-string "~max")
;; => (clojure.core/unquote max)

(shuffle-expr '(= X.a Y.b))
;; => "(X.a = Y.b)"

(shuffle-expr '(AND (< a 5) (< b ~max)))
;; => "((a < 5) AND (b < ?))"

(shuffle-expr '(AND (< a 5) (OR (> b 0) (< b ~max))))
;; => "((a < 5) AND ((b > 0) OR (b < ?)))"

(defn process-where-clause [processor expr]
  (str " WHERE " (processor expr)))

(process-where-clause shuffle-expr '(AND (< a 5) (< b ~max)))
;; => " WHERE ((a < 5) AND (b < ?))"

(defn process-left-join-clause [processor table _ expr]
  (str " LEFT JOIN " table " ON " (processor expr)))

(apply process-left-join-clause shuffle-expr '(Y :ON (= X.a Y.b)))
;; => " LEFT JOIN Y ON (X.a = Y.b)"

(defn process-from-clause [processor table & joins]
  (apply str " FROM " table (map processor joins)))

(process-from-clause shuffle-expr 'X
                     (process-left-join-clause shuffle-expr 'Y :ON
                                               '(= X.a Y.b)))
;; => " FROM X LEFT JOIN Y ON (X.a = Y.b)"

(defn process-select-clause [processor fields & clauses]
  (apply str "SELECT " (str/join ", " fields)
         (map processor clauses)))

(process-select-clause shuffle-expr
    '[a b c]
    (process-from-clause shuffle-expr 'X
                         (process-left-join-clause shuffle-expr 'Y :ON '(= X.a Y.b)))
    (process-where-clause shuffle-expr '(AND (< a 5) (< b ~max))))
;; => "SELECT a, b, c FROM X LEFT JOIN Y ON (X.a = Y.b) WHERE ((a < 5) AND (b < ?))"

(declare apply-syntax)

(def ^:dynamic *clause-map*
  {'SELECT (partial process-select-clause apply-syntax)
  'FROM (partial process-from-clause apply-syntax)
  'LEFT-JOIN (partial process-left-join-clause shuffle-expr)
  'WHERE (partial process-where-clause shuffle-expr)})

(defn apply-syntax [[op & args]]
  (apply (get *clause-map* op) args))

(defmacro SELECT [& args]
  {:query (apply-syntax (cons 'SELECT args))
          :bindings (vec (for [n (tree-seq coll? seq args)
                               :when (and (coll? n)
                                          (= (first n) `unquote))]
                           (second n)))})

(defn example-query [max]
  (SELECT [a b c]
          (FROM X
                (LEFT-JOIN Y :ON (= X.a Y.b)))
          (WHERE (AND (< a 5) (< b ~max)))))

(example-query 9)
;; => {:query "SELECT a, b, c FROM X LEFT JOIN Y ON (X.a = Y.b) WHERE ((a < 5) AND (b < ?))", :bindings [9]}
