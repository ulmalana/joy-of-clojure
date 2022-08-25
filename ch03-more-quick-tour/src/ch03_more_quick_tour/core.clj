(ns ch03-more-quick-tour.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;;;;;;;;;;;;;;;;;; truthiness ;;;;;;;;;;;;;;;;;;;;
;; everything is true, except for false and nil
(if true :truthy :falsey) ; :truthy
(if [] :truthy :falsey) ; truthy
(if nil :truthy :falsey) ; :falsey
(if false :truthy :false) ; :falsey

;; nil punning

(seq [1 2 3])

;; check if collection is empty.
;; better for branching
(seq [])

(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

;;;;;;;;;;; destructuring ;;;;;;;;;;;;;;;;;;;;;;;;;;

(def guys-whole-name ["Guy" "Lewis" "Steele"])

;; without destructuring
(str (nth guys-whole-name 2) ", "
     (nth guys-whole-name 0) " "
     (nth guys-whole-name 1))

;; destructuring with a vector
(let [[f-name m-name l-name] guys-whole-name]
  (str l-name ", " f-name " " m-name))

;; destructuring with map
(def guys-name-map
  {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})

(let [{f-name :f-name, m-name :m-name, l-name :l-name} guys-name-map]
  (str l-name ", " f-name " " m-name))

;; with :keys
(let [{:keys [f-name m-name l-name]} guys-name-map]
  (str l-name ", " f-name " " m-name))

;; with :as
(let [{f-name :f-name, :as whole-name} guys-name-map]
  (println "First name is " f-name)
  (println "Whole name is: ")
  whole-name)

;; provide default binding with :or
(let [{:keys [title f-name m-name l-name],
       :or {title "Mr."}} guys-name-map]
  (println title f-name m-name l-name))

;; associative destructuring
(let [{first-thing 0, last-thing 3} [1 2 3 4]]
  [first-thing last-thing])

;;;;;;;;;;;;;;; using REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (rem (bit-xor x y) 256)]))

(def frame (java.awt.Frame.))

(.setVisible frame true)
(.setSize frame (java.awt.Dimension. 200 200))

(def gfx (.getGraphics frame))
(.fillRect gfx 100 100 50 75)

(.setColor gfx (java.awt.Color. 255 128 0))
(.fillRect gfx 100 150 75 50)

(doseq [[x y xor] (xors 200 200)]
  (.setColor gfx (java.awt.Color. xor xor xor))
  (.fillRect gfx x y 1 1))

(defn clear [g] (.clearRect g 0 0 200 200))

(defn f-values [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw-values [f xs ys]
  (clear gfx)
  (.setSize frame (java.awt.Dimension. xs ys))
  (doseq [[x y v] (f-values f xs ys)]
    (.setColor gfx (java.awt.Color. v v v))
    (.fillRect gfx x y 1 1)))
