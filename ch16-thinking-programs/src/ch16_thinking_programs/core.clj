(ns ch16-thinking-programs.core
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.fd :as fd]
            [clojure.core.logic :as logic])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])

(defn prep [board]
  (map #(partition 3 %)
       (partition 9 board)))

(defn print-board
  [board]
  (let [row-sep (apply str (repeat 37 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (print "| ")
      (doseq [subrow (nth board row)]
        (doseq [cell (butlast subrow)]
          (print (str cell "   ")))
        (print (str (last subrow) " | ")))
      (println)
      (when (zero? (mod (inc row) 3))
        (println row-sep)))))

(-> b1 prep print-board)
;; -------------------------------------
;; | 3   -   - | -   -   5 | -   1   - | 
;; | -   7   - | -   -   6 | -   3   - | 
;; | 1   -   - | -   9   - | -   -   - | 
;; -------------------------------------
;; | 7   -   8 | -   -   - | -   9   - | 
;; | 9   -   - | 4   -   8 | -   -   2 | 
;; | -   6   - | -   -   - | 5   -   1 | 
;; -------------------------------------
;; | -   -   - | -   4   - | -   -   6 | 
;; | -   4   - | 7   -   - | -   2   - | 
;; | -   2   - | 6   -   - | -   -   3 | 
;; -------------------------------------
;; => nil

(defn rows [board size]
  (partition size board))

(defn row-for [board index size]
  (nth (rows board size) (/ index 9)))

(row-for b1 1 9)
;; => (3 - - - - 5 - 1 -)

(defn column-for [board index size]
  (let [col (mod index size)]
    (map #(nth % col)
         (rows board size))))

(column-for b1 2 9)
;; => (- - - 8 - - - - -)

(defn subgrid-for [board i]
  (let [rows (rows board 9)
        sgcol (/ (mod i 9) 3)
        sgrow (/ (/ i 9) 3)
        grp-col (column-for (mapcat #(partition 3 %) rows) sgcol 3)
        grp (take 3 (drop (* 3 (int sgrow)) grp-col))]
    (flatten grp)))

(subgrid-for b1 0)
;; => (3 - - - 7 - 1 - -)

(defn numbers-present-for [board i]
  (set
   (concat (row-for board i 9)
           (column-for board i 9)
           (subgrid-for board i))))

(numbers-present-for b1 1)
;; => #{7 1 4 - 6 3 2 5}

(defn possible-placements [board index]
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers-present-for board index)))

(defn index [coll]
  (cond
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)]
    i))

(defn solve [board]
  (if-let [[i & _]
           (and (some '#{-} board)
                (pos '#{-} board))]
    (flatten (map #(solve (assoc board i %))
                  (possible-placements board i)))
    board))

(-> b1 solve prep print-board)
;; -------------------------------------
;; | 3   8   6 | 2   7   5 | 4   1   9 | 
;; | 4   7   9 | 8   1   6 | 2   3   5 | 
;; | 1   5   2 | 3   9   4 | 8   6   7 | 
;; -------------------------------------
;; | 7   3   8 | 5   2   1 | 6   9   4 | 
;; | 9   1   5 | 4   6   8 | 3   7   2 | 
;; | 2   6   4 | 9   3   7 | 5   8   1 | 
;; -------------------------------------
;; | 8   9   3 | 1   4   2 | 7   5   6 | 
;; | 6   4   1 | 7   5   3 | 9   2   8 | 
;; | 5   2   7 | 6   8   9 | 1   4   3 | 
;; -------------------------------------
;; => nil

(defn lvar?
  "Determines if a value represents a logic variable"
  [x]
  (boolean
   (when (symbol? x)
     (re-matches #"^\?.*" (name x)))))

(lvar? '?x)
;; => true

(lvar? 'a)
;; => false

(lvar? 2)
;; => false

(defn satisfy1
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
      (= L R) knowledge
      (lvar? L) (assoc knowledge L R)
      (lvar? R) (assoc knowledge R L)
      :default nil)))

(satisfy1 '?something 2 {})
;; => {?something 2}

(satisfy1 2 '?something {})
;; => {?something 2}

(satisfy1 '?x '?y {})
;; => {?x ?y}

(->> {}
     (satisfy1 '?x '?y)
     (satisfy1 '?x 1))
;; => {?x ?y, ?y 1}

(defn satisfy
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
      (not knowledge) nil
      (= L R) knowledge
      (lvar? L) (assoc knowledge L R)
      (lvar? R) (assoc knowledge R L)
      (every? seq? [L R])
         (satisfy (rest L)
                  (rest R)
                  (satisfy (first L)
                           (first R)
                           knowledge))
      :default nil)))

(satisfy '(1 2 3) '(1 ?something 3) {})
;; => {?something 2}

(satisfy '((((?something)))) '((((2)))) {})
;; => {?something 2}

(satisfy '(?x 2 3 (4 5 ?z))
         '(1 2 ?y (4 5 6))
         {})
;; => {?x 1, ?y 3, ?z 6}

(satisfy '?x '(?y) {})
;; => {?x (?y)}

(satisfy '(?x 10000 3) '(1 2 ?y) {})
;; => nil

(defn subst [term binds]
  (walk/prewalk
   (fn [expr]
     (if (lvar? expr)
       (or (binds expr) expr)
       expr))
   term))

(subst '(1 ?x 3) '{?x 2})
;; => (1 2 3)

(subst '((((?x)))) '{?x 2})
;; => ((((2))))

(subst '[1 ?x 3] '{?x 2})
;; => [1 2 3]

(subst '{:a ?x :b [1 ?x 3]} '{?x 2})
;; => {:a 2, :b [1 2 3]}

(subst '(1 ?x 3) '{})
;; => (1 ?x 3)

(subst '(1 ?x 3) '{?x ?y})
;; => (1 ?y 3)
;; => 

(def page
  '[:html
    [:head [:title ?title]]
    [:body [:h1 ?title]]])

(subst page '{?title "Hehe"})
;; => [:html [:head [:title "Hehe"]] [:body [:h1 "Hehe"]]]

(defn meld [term1 term2]
  (->> {}
       (satisfy term1 term2)
       (subst term1)))

(meld '(1 ?x 3) '(1 2 ?y))
;; => (1 2 3)

(meld '(1 ?x) '(?y (?y 2)))
;; => (1 (1 2))

;;;;;;;;;;; core.logic ;;;;;;;;;;;;;;
(logic/run* [answer] (logic/== answer 5))
;; => (5)

(logic/run* [val1 val2]
  (logic/== {:a val1, :b 2}
            {:a 1, :b val2}))
;; => ([1 2])

(logic/run* [x y]
  (logic/== x y))
;; => ([_0 _0])

(logic/run* [q]
  (logic/== q 1)
  (logic/== q 2))
;; => ()

(logic/run* [george]
  (logic/conde
   [(logic/== george :born)]
   [(logic/== george :unborn)]))
;; => (:born :unborn)

(pldb/db-rel orbits orbital body)
;; => #'ch16-thinking-programs.core/orbits

(def facts
  (pldb/db
   [orbits :mercury :sun]
   [orbits :venus :sun]
   [orbits :earth :sun]
   [orbits :mars :sun]
   [orbits :jupiter :sun]
   [orbits :saturn :sun]
   [orbits :uranus :sun]
   [orbits :neptune :sun]))
;; => #'ch16-thinking-programs.core/facts

(pldb/with-db facts
  (doall (logic/run* [q]
           (logic/fresh [orbital body]
             (orbits orbital body)
             (logic/== q orbital)))))
;; => (:saturn :earth :uranus :neptune :mars :jupiter :venus :mercury)


;;;;;; constraint programming ;;;;;;
(defn doubler [n] (* n 2))

(defn rowify [board]
  (->> board
       (partition 9)
       (map vec)
       vec))


(rowify b1)
;; [[3 - - - - 5 - 1 -]
;;  [- 7 - - - 6 - 3 -]
;;  [1 - - - 9 - - - -]
;;  [7 - 8 - - - - 9 -]
;;  [9 - - 4 - 8 - - 2]
;;  [- 6 - - - - 5 - 1]
;;  [- - - - 4 - - - 6]
;;  [- 4 - 7 - - - 2 -]
;;  [- 2 - 6 - - - - 3]]

(defn colify [rows]
  (apply map vector rows))


(colify (rowify b1))
;; => ([3 - 1 7 9 - - - -] [- 7 - - - 6 - 4 2] [- - - 8 - - - - -] [- - - - 4 - - 7 6] [- - 9 - - - 4 - -] [5 6 - - 8 - - - -] [- - - - - 5 - - -] [1 3 - 9 - - - 2 -] [- - - - 2 1 6 - 3])

(defn subgrid [rows]
  (partition 9
             (for [row (range 0 9 3)
                   col (range 0 9 3)
                   x (range row (+ row 3))
                   y (range col (+ col 3))]
               (get-in rows [x y]))))

(subgrid (rowify b1))
;; => ((3 - - - 7 - 1 - -) (- - 5 - - 6 - 9 -) (- 1 - - 3 - - - -) (7 - 8 9 - - - 6 -) (- - - 4 - 8 - - -) (- 9 - - - 2 5 - 1) (- - - - 4 - - 2 -) (- 4 - 7 - - 6 - -) (- - 6 - 2 - - - 3))

(def logic-board #(repeatedly 81 logic/lvar))

(defn init [[lv & lvs] [cell & cells]]
  (if lv
    (logic/fresh []
      (if (= '- cell)
        logic/succeed
        (logic/== lv cell))
      (init lvs cell))
    logic/succeed))

(defn solve-logically [board]
  (let [legal-nums (fd/interval 1 9)
        lvars (logic-board)
        rows (rowify lvars)
        cols (colify rows)
        grids (subgrid rows)]
    (logic/run 1 [q]
      (init lvars board)
      (logic/everyg #(fd/in % legal-nums) lvars)
      (logic/everyg fd/distinct rows)
      (logic/everyg fd/distinct cols)
      (logic/everyg fd/distinct grids)
      (logic/== q lvars))))

;; error
;; (-> b1
;;     solve-logically
;;     first
;;     prep
;;     print-board)
;; => 
;; => 
