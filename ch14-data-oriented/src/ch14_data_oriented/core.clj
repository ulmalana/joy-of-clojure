(ns ch14-data-oriented.core
  (:use clojure.data)
  (:require [clojure.edn :as edn])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def ascii (map char (range 65 (+ 65 26))))

(defn rand-str
  [size alphabet]
  (apply str (repeatedly size #(rand-nth alphabet))))

(rand-str 10 ascii)
;; => "LQUGLOMEDK"

(def rand-sym #(symbol (rand-str %1 %2)))

(def rand-key #(keyword (rand-str %1 %2)))

(rand-key 10 ascii)
;; => :SQIUOZDUGP

(rand-sym 10 ascii)
;; => NQIDGSKEGU

(defn rand-vec
  [& generators]
  (into [] (map #(%) generators)))

(rand-vec #(rand-sym 5 ascii)
          #(rand-key 10 ascii)
          #(rand-int 1024))
;; => [PABFI :CWFJUEQNCC 239]

(defn rand-map
  [size kgen vgen]
  (into {}
        (repeatedly size #(rand-vec kgen vgen))))

(rand-map 3 #(rand-key 5 ascii) #(rand-int 100))
;; => {:OFXOJ 16, :UROIH 78, :SJRVB 33}

(assert (= [1 2 3] (conj [1 2] 3)))
;; => nil

(diff [1 2 3] [1 2 4])
;; => [[nil nil 3] [nil nil 4] [1 2]]

(defn filter-rising
  [segments]
  (clojure.set/select
   (fn [{:keys [p1 p2]}]
     (> 0
        (/ (- (p2 0) (p1 0))
           (- (p2 1) (p1 1)))))
   segments))

(filter-rising #{{:p1 [0 0] :p2 [1 1]}
                 {:p1 [4 15] :p2 [3 21]}})
;; => #{{:p1 [4 15], :p2 [3 21]}}

(edn/read-string "#uuid \"dae78a90-d491-11e2-8b8b-0800200c9a66\"")
;; => #uuid "dae78a90-d491-11e2-8b8b-0800200c9a66"

(edn/read-string "42")
;; => 42

(edn/read-string "{:a 42 \"b\" 36 [:c] 9}")
;; => {:a 42, "b" 36, [:c] 9}
