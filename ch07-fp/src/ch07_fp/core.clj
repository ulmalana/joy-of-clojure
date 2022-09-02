(ns ch07-fp.core
  (:use [clojure.test :as t])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; function composition
(def fifth (comp first rest rest rest rest))
(fifth [1 2 3 4 5])

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))
((fnth 5) '[a b c d e])

;; list -> keyword
(map (comp
      keyword
      #(.toLowerCase %)
      name)
     '(a B c))

;; partial function
((partial + 5) 100 150)

;; complement: return the opposite truhty value
(let [truthiness (fn [v] v)]
  [((complement truthiness) true)
   ((complement truthiness) 42)
   ((complement truthiness) false)
   ((complement truthiness) nil)])

((complement even?) 2) ;; similar to (comp not even?)

;; function as data
(defn join
  ;; function metadata for testing
  {:test (fn []
           (assert
            (= (join "," [1 2 3]) "1,2,3")))}

  ;; function definition
  [sep s]
  (apply str (interpose sep s)))

;; run the test
(t/run-tests)

;; higher order functions
;; functions as arguments
(sort [7 5 1 3])

;; sort in descending. only works when the elements are comparable
(sort > [7 5 1 3])

;; alternative: sort-by
(sort-by second [[:a 7] [:c 13] [:b 21]])
(sort-by str ["z" "x" "a" "aa" 1 2 3])
(sort-by :age [{:age 99} {:age 14} {:age 24}])

(def plays [{:band "Burial" :plays 979 :loved 9}
            {:band "Eno" :plays 2333 :loved 15}
            {:band "Bill Evans" :plays 979 :loved 9}
            {:band "Magma" :plays 2665 :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(sort-by-loved-ratio plays)


;; functions as return values
(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))

;; pure functions
;; manipulate the value of map
(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only)
            (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band} (plays 0))

(defn manip-map [f ks m]
  (merge m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))

;; not referentially transparent
(defn mega-love! [ks]
  (map (partial manip-map #(int (* % 1000)) ks) plays))

(mega-love! [:loved])


;; named arguments with destructuring
(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

;; constraing functions with pre and postconditions
(defn slope' [p1 p2]
  ;; preconditions
  ;; both p1 and p2 must be vector and not the same
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   ;; post conditions
   ;; the output must be float
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

;; unconstrained function
(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))
(put-things {})


;; constraining the map
(defn vegan-constraints [f m]
  {:pre [(:veggie m)]
   :post [(:veggie %) (nil? (:meat %))]}
  (f m))

;; this will throw exception
;;(vegan-constraints put-things {:veggie "carrot"})

;; more constraining
(defn balanced-diet [f m]
  {:post [(:meat %) (:veggie %)]}
  (f m))

(defn finicky [f m]
  {:post [(= (:meat %) (:meat m))]}
  (f m))

;; closure
;; a function that has access to locals from the context
;; where it was created
(def times-two
  (let [x 2]
    (fn [y] (* y x))))

(times-two 4)

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(add-and-get 2)
(add-and-get 2)
(add-and-get 7)

(defn times-n [n]
  (let [x n]
    (fn [y] (* y x))))

(def times-four (times-n 4))
(times-four 6)

(defn times-n' [n]
  (fn [y] (* y n)))

(def times-six (times-n' 6))
(times-six 9)

(defn divisible [denom]
  (fn [num]
    (zero? (rem num denom))))

((divisible 3) 6)
((divisible 3) 7)

;; passing closures as functions
(filter even? (range 10))

(filter (divisible 4) (range 10))

(defn filter-divisible [denom s]
  (filter (fn [num] (zero? (rem num denom))) s))

(filter-divisible 4 (range 10))

(defn filter-divisible' [denom s]
  (filter #(zero? (rem % denom)) s))
(filter-divisible 6 (range 100))


;; robot
(def bearings [{:x 0 :y 1} ;; north : num 0
               {:x 1 :y 0} ;; east  : num 1
               {:x 0 :y -1} ;; south : num 2
               {:x -1 :y 0}]) ;; west : num 3

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

;; move to the north (0) from starting point (5,5)
(forward 5 5 0)

;; move to the south (2) from starting point (5,5)
(forward 5 5 2)

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                        (+ y (:y (bearings bearing-num)))
                        bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:coords (bot 5 5 0))
(:bearing (bot 5 5 2))
(:coords ((:forward (bot 5 5 0))))

;; from (5,5), turn right (to east), move two steps forward
(:bearing ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))
(:coords ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))

;; mirror bot
(defn mirror-bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                               (- y (:y (bearings bearing-num)))
                               bearing-num))
   :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
   :turn-left (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})

;;;;;; recursion ;;;;;;;;;;;;;;

;; mundane recursion
;; this will overflow
(defn pow [base exp]
  (if (zero? exp)
    1
    (* base (pow base (dec exp)))))

(pow 2 10)
(pow 1.01 925)
;; (pow 2 1000) will overflow

;; better version
;; with tail-recursive
(defn pow' [base exp]
  (letfn [(kapow [base exp acc]
            (if (zero? exp)
              acc
              (recur base (dec exp) (* base acc))))]
    (kapow base exp 1)))
(pow' 2N 1000)

;; simple converter
(def simple-metric {:meter 1,
                    :km 1000,
                    :cm 1/100,
                    :mm [1/10 :cm]})

(-> (* 3 (:km simple-metric))
    (+ (* 10 (:meter simple-metric)))
    (+ (* 80 (:cm simple-metric)))
    (+ (* (:cm simple-metric)
          (* 10 (first (:mm simple-metric)))))
    float)

(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(convert simple-metric [1 :meter])
(convert simple-metric [50 :cm])
(float (convert simple-metric [3 :km 10 :meter 80 :cm 10 :mm]))
(convert {:bit 1, :byte 8, :nibble [1/2 :byte]} [32 :nibble])


(defn gcd [x y]
  (cond
    (> x y) (gcd (- x y) y)
    (< x y) (gcd x (- y x))
    :else x))

;;; elevator's finite state machines
(defn elevator [commands]
  (letfn
      [(ff-open [[_ & r]]
         "When the elevator is open on the first floor, it can either close or be done"
         #(case _
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[_ & r ]]
         "When the elevator is closed on the first floor, it can either open or go up"
         #(case _
            :open (ff-open r)
            :up (sf-closed r)
            false))
       (sf-closed [[_ & r]]
         "When the elevator is closed on the second floor, it can either go down or open"
         #(case _
            :down (ff-closed r)
            :open (sf-open r)
            false))
       (sf-open [[_ & r]]
         "When the elevator is open on the second floor, it can either close or be done"
         #(case _
            :close (sf-closed r)
            :done true
            false))]
    (trampoline ff-open commands)))

(elevator [:close :open :close :up :open :open :done])
(elevator [:close :up :open :close :down :open :done])


;; continuation passing style
(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))
(defn fac [n]
  (fac-cps n identity))

(fac 10)

(defn mk-cps [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v]
                    (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def fac'
  (mk-cps zero?
          identity
          #(* %1 %2)))

(fac' 10)


(def tri
  (mk-cps #(== 1 %)
          identity
          #(+ %1 %2)))
(tri 10)

;;;;;;;;; A* algorithm ;;;;;;;;;;;;;;
(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

;; from listing 5.1
(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(estimate-cost 900 5 0 0)
(estimate-cost 900 5 4 4)

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(path-cost 900 {:cost 1})

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(total-cost 1000 900 5 3 4)

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))
(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                    (keep #(get-in routes %)
                                          nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                         (map
                          (fn [w]
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(astar [0 0]
       900
       world)

(astar [0 0]
       900
       [[1 1 1 2 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 1 1 1]])

(astar [0 0]
       900
       [[1 1 1 2 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 999 1]
        [1 1 1 666 1]])
