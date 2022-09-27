(ns ch15-performance.core
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
