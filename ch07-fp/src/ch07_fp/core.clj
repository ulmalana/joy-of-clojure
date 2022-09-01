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
