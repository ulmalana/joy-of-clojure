(ns ch09-combine-data-code.core
  (:use [ch09-combine-data-code.ns-ex]
        [ch09-combine-data-code.udp])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(in-ns 'joy.ns)
(def authors ["Chouser"])

(in-ns 'your.ns)
(clojure.core/refer 'joy.ns)

joy.ns/authors
;; => ["Chouser"]

(in-ns 'joy.ns)
(def authors ["Chouser" "Fogus"])

(in-ns 'your.ns)
joy.ns/authors
;; => ["Chouser" "Fogus"]

;;;;;;;;;;; create namespace ';;;;;;;;;;;;;;;;;;;;;;;

;; ns macro
;; automatically include java.lang and clojure.core
(ns chimp)
(reduce + [1 2 (Integer. 3)])
;; => 6

;; in-ns function
;; only include java.lang
(in-ns 'gibbon)

;; will throw exception because reduce is unknown
;;(reduce + [1 2 (Integer. 3)])

(clojure.core/refer 'clojure.core)
(reduce + [1 2 (Integer. 3)])
;; => 6

;; create-ns
;; only create a namespace map.
;; need to refer to other namespace.
(def b (create-ns 'bonobo))
b
;; => #namespace[bonobo]

((ns-map b) 'String);; => java.lang.String
(intern b 'x 9)
;; => #'bonobo/x
bonobo/x
;; => 9

(intern b 'reduce clojure.core/reduce)
;; => #'bonobo/reduce
(intern b '+ clojure.core/+)
;; => #'bonobo/+
(in-ns 'bonobo)
;; => #namespace[bonobo]
(reduce + [1 2 3 4 5])
;; => 15

(in-ns 'ch09-combine-data-code.core)
;; => #namespace[ch09-combine-data-code.core]

(get (ns-map 'bonobo) 'reduce)
;; => #'bonobo/reduce

(ns-unmap 'bonobo 'reduce)
;; => nil
(get (ns-map 'bonobo) 'reduce)
;; => nil

(remove-ns 'bonobo)
;; => #namespace[bonobo]
(all-ns)


;;;;;;;;;;;;;;;;;;;;;;;; multimethods ;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti compiler :os)

(defmethod compiler ::unix
  [m]
  (get m :c-compiler))

(defmethod compiler ::osx
  [m]
  (get m :llvm-compiler))

(def clone (partial beget {}))

(def unix {:os ::unix, :c-compiler "cc", :home "/home", :dev "/dev"})
(def osx (-> (clone unix)
             (put :os ::osx)
             (put :llvm-compiler "clang")
             (put :home "/Users")))

(compiler unix)
;; => "cc"

(compiler osx)
;; => "clang"

(defmulti home :os)

(defmethod home ::unix
  [m]
  (get m :home))

(home unix)
;; => "/home"

(derive ::osx ::unix) ;; :osx is a ::unix

(home osx)
;; => "/Users"

(parents ::osx)
;; => #{:ch09-combine-data-code.core/unix}

(ancestors ::osx)
;; => #{:ch09-combine-data-code.core/unix}

(descendants ::unix)
;; => #{:ch09-combine-data-code.core/osx}

(isa? ::osx ::unix)
;; => true

(isa? ::unix ::osx)
;; => false

(defmulti compile-cmd (juxt :os compiler))

(defmethod compile-cmd [::osx "clang"]
  [m]
  (str "/usr/bin/" (get m :llvm-compiler)))

(defmethod compile-cmd :default
  [m]
  (str "Unsure where to locate " (get m :c-compiler)))

(compile-cmd osx)
;; => "/usr/binclang"

(compile-cmd unix)
;; => "Unsure where to locate cc"

;;;;;;;;;;;;;;;;; records ;;;;;;;;;;;;;;;;;;;;;;;
(defrecord TreeNode [val l r])

(TreeNode. 5 nil nil)
;; => #ch09_combine_data_code.core.TreeNode{:val 5, :l nil, :r nil}

(defn xconj [t v]
  (cond
    (nil? t) (TreeNode. v nil nil)
    (< v (:val t)) (TreeNode. (:val t) (xconj (:l t) v) (:r t))
    :else (TreeNode. (:val t) (:l t) (xconj (:r t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:l t)) [(:val t)] (xseq (:r t)))))

(def sample-tree
  (reduce xconj nil [3 5 2 4 6]))

(xseq sample-tree)
;; => (2 3 4 5 6)

;; return plain map, not record
(dissoc (TreeNode. 5 nil nil) :l)
;; => {:val 5, :r nil}

;;;;;;;;;;;;;;;;;;;; protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol FIXO
  (fixo-push [fixo value])
  (fixo-pop [fixo])
  (fixo-peek [fixo]))

(extend-type TreeNode
  FIXO
  (fixo-push [node value]
    (xconj node value))
  (fixo-peek [node]
    (if (:l node)
      (recur (:l node))
      (:val node)))
  (fixo-pop [node]
    (if (:l node)
      (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
      (:r node))))


(xseq (fixo-push sample-tree 5/2)) 
;; => (2 5/2 3 4 5 6)

(extend-type clojure.lang.IPersistentVector
  FIXO
  (fixo-push [vector value]
    (conj vector value))
  (fixo-peek [vector]
    (peek vector))
  (fixo-pop [vector]
    (pop vector)))

(fixo-push [2 3 4 5 6] 5/2)
;; => [2 3 4 5 6 5/2]

;; (reduce fixo-push nil [3 5 2 4 6 0])
;; => No implementation of method :fixo-push found for class: nil

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))

(xseq (reduce fixo-push nil [3 5 2 4 6 0]))
;; => (0 2 3 4 5 6)

(defn fixo-into [c1 c2]
  (reduce fixo-push c1 c2))

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
;; => (2 4 5 6 7)

(seq (fixo-into [5] [2 4 6 7]))
;; => (5 2 4 6 7)

(def tree-node-fixo
  {:fixo-push (fn [node value]
                (xconj node value))
   :fixo-peek (fn [node]
               (if (:l node)
                 (recur (:l node))
                 (:val node)))
   :fixo-pop (fn [node]
               (if (:l node)
                 (TreeNode. (:val node) (fixo-pop (:l node)) (:r node))
                 (:r node)))})

(extend TreeNode FIXO tree-node-fixo)

(xseq (fixo-into (TreeNode. 5 nil nil) [2 4 6 7]))
;; => (2 4 5 6 7)

;;; reify macor
(defn fixed-fixo
  ([limit] (fixed-fixo limit []))
  ([limit vector]
   (reify FIXO
     (fixo-push [this value]
       (if (< (count vector) limit)
         (fixed-fixo limit (conj vector value))
         this))
     (fixo-peek [_]
       (peek vector))
     (fixo-pop [_]
       (pop vector)))))

;;;; implementing protocol directly in record definition
(defrecord TreeNode [val l r]
  FIXO
  (fixo-push [t v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [t]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [t]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r)))

(def sample-tree2 (reduce fixo-push (TreeNode. 3 nil nil) [5 2 4 6]))

(xseq sample-tree2)
;; => (2 3 4 5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;; type ;;;;;;;;;;;;;;;;;;;;;;;;;
;; type is similar to record but doesnt include any method.

(deftype InfiniteConstant [i]
  clojure.lang.ISeq ;; extend from ISeq interface
  (seq [this] ;; creating our own seq for this type. wont conflict with the real seq.
    (lazy-seq (cons i (seq this)))))

(take 3 (InfiniteConstant. 5))
;; => (5 5 5)

(deftype TreeNode [val l r]
  FIXO  ;; implement FIXO protocol
  (fixo-push [_ v]
    (if (< v val)
      (TreeNode. val (fixo-push l v) r)
      (TreeNode. val l (fixo-push r v))))
  (fixo-peek [_]
    (if l
      (fixo-peek l)
      val))
  (fixo-pop [_]
    (if l
      (TreeNode. val (fixo-pop l) r)
      r))

  clojure.lang.IPersistentStack ;; implement interface
  (cons [this v]
    (fixo-push this v))
  (peek [this]
    (fixo-peek this))
  (pop [this]
    (fixo-pop this))

  clojure.lang.Seqable   ;; implement interface
  (seq [t]
    (concat (seq l) [val] (seq r))))

(extend-type nil
  FIXO
  (fixo-push [t v]
    (TreeNode. v nil nil)))

(def sample-tree3 (into (TreeNode. 3 nil nil) [5 2 4 6]))
(seq sample-tree3)
;; => (2 3 4 5 6)

;;;;;;;;;;;; chess move ;;;;;;;;;;;;;;;

(defn build-move [& pieces]
  (apply hash-map pieces))

(build-move :from "e7" :to "e8" :promotion \Q)
;; => {:from "e7", :promotion \Q, :to "e8"}

(defrecord Move [from to castle? promotion]
  Object
  (toString [this]
    (str "Move " (:from this)
         " to " (:to this)
         (if (:castle? this) " castle"
             (if-let [p (:promotion this)]
               (str " promote to " p)
               "")))))

(str (Move. "e2" "e4" nil nil))
;; => "Move e2 to e4"

(.println System/out (Move. "e7" "e8" nil \Q))
;; => Move e7 to e8 promote to Q

(defn build-move' [& {:keys [from to castle? promotion]}]
  {:pre [from to]}
  (Move. from to castle? promotion))

(str (build-move' :from "e2" :to "e4"))
;; => "Move e2 to e4"
