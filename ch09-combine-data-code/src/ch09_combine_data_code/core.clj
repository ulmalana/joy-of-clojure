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
