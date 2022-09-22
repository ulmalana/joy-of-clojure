(ns ch12-java.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream]
           [java.util Comparator Collections ArrayList]
           [java.util.concurrent FutureTask])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
   (respond identity exchange body))
  ([around exchange body]
   (.sendResponseHeaders exchange OK 0)
   (with-open [resp (around (.getResponseBody exchange))]
     (.write resp (.getBytes body)))))

(defn new-server
  [port path handler]
  (doto
      (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler
  [txt]
  (proxy [HttpHandler]  ;; extend
      []                ;; no actor args
    (handle [exchange]  ;; override
      (respond exchange txt))))

;; (def server
;;   (new-server
;;    8123
;;    "/joy/hello"
;;    (default-handler "Hello Jakarta")))

(def p (default-handler
        "There's no problem that cant be solved with another level of indirection"))

(def server (new-server 8123 "/" p))

(update-proxy p
              {"handle" (fn [this exchange]
                          (respond exchange (str "this is " this)))})

(def echo-handler
  (fn [_ exchange]
    (let [headers (.getRequestHeaders exchange)]
      (respond exchange (prn-str headers)))))

(update-proxy p {"handle" echo-handler})

(defn html-around
  [o]
  (proxy [FilterOutputStream]
      [o]
    (write [raw-bytes]
      (proxy-super write
                   (.getBytes (str "<html><body>"
                                   (String. raw-bytes)
                                   "</body></html>"))))))

(defn listing
  [file]
  (-> file .list sort))

(listing (io/file "."))
;; => (".gitignore" ".hgignore" ".nrepl-port" "CHANGELOG.md" "LICENSE" "README.md" "doc" "project.clj" "resources" "src" "target" "test")

(listing (io/file "./README.md"))
;; => ()

(defn html-links
  [root filenames]
  (string/join
   (for [file filenames]
     (str "<a href='"
          (str root
               (if (= "/" root)
                 ""
                 File/separator)
               file)
          "'>"
          file "</a><br>"))))

(html-links "." (listing (io/file ".")))
;; => "<a href='./.gitignore'>.gitignore</a><br><a href='./.hgignore'>.hgignore</a><br><a href='./.nrepl-port'>.nrepl-port</a><br><a href='./CHANGELOG.md'>CHANGELOG.md</a><br><a href='./LICENSE'>LICENSE</a><br><a href='./README.md'>README.md</a><br><a href='./doc'>doc</a><br><a href='./project.clj'>project.clj</a><br><a href='./resources'>resources</a><br><a href='./src'>src</a><br><a href='./target'>target</a><br><a href='./test'>test</a><br>"

(defn details
  [file]
  (str (.getName file) " is "
       (.length file) " bytes."))
;; => #'ch12-java.core/details

(details (io/file "./README.md"))
;; => "README.md is 21 bytes."

(defn uri->file
  [root uri]
  (->> uri
       str
       URLDecoder/decode
       (str root)
       io/file))
;; => #'ch12-java.core/uri->file

(uri->file "." (URI. "/project.clj"))
;; => #object[java.io.File 0x7b196452 "./project.clj"]

(details (uri->file "." (URI. "/project.clj")))
;; => "project.clj is 473 bytes."

(def fs-handler
  (fn [_ exchange]
    (let [uri (.getRequestURI exchange)
          file (uri->file "." uri)]
      (if (.isDirectory file)
        (do (.add (.getResponseHeaders exchange)
                  "Content-Type" "text/html")
            (respond html-around
                     exchange
                     (html-links (str uri) (listing file))))
        (respond exchange (details file))))))

(update-proxy p {"handle" fs-handler})

(doto (StringBuilder. "abc")
  (.append (into-array [\x \y \z])))
;; => #object[java.lang.StringBuilder 0x7c705b3a "abc[Ljava.lang.Character;@7fa629fa"]

(doto (StringBuilder. "abc")
  (.append (char-array [\x \y \z])))
;; => #object[java.lang.StringBuilder 0x6da19eb3 "abcxyz"]

(let [ary (make-array Long/TYPE 3 3)]
  (dotimes [i 3]
    (dotimes [j 3]
      (aset ary i j (+ i j))))
  (map seq ary))
;; => ((0 1 2) (1 2 3) (2 3 4))

(into-array Integer/TYPE [1 2 3])
;; => #object["[I" 0x5f8e6ce1 "[I@5f8e6ce1"]

(def ary (into-array [1 2 3]))
;; => #'ch12-java.core/ary
(def sary (seq ary))
;; => #'ch12-java.core/sary
sary
;; => (1 2 3)

(aset ary 0 42)
;; => 42

sary
;; => (42 2 3)

(defn asum-sq
  [xs]
  (let [db1 (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce db1 i ret 0
             (+ ret (aget db1 i)))))
;; => #'ch12-java.core/asum-sq

(defmulti what-is class)
;; => #'ch12-java.core/what-is
(defmethod what-is
  (Class/forName "[Ljava.lang.String;")
  [_]
  "1d String")
;; => #multifn[what-is 0x440212f4]

(defmethod what-is
  (Class/forName "[[Ljava.lang.Object;")
  [_]
  "2d Object")
;; => #multifn[what-is 0x440212f4]

(defmethod what-is
  (Class/forName "[[[[I")
  [_]
  "Primitive 4d int")
;; => #multifn[what-is 0x440212f4]

(defmethod what-is
  (Class/forName "[[D")
  [a]
  "Primitive 2d double")
;; => #multifn[what-is 0x440212f4]

(defmethod what-is
  (Class/forName
   "[Lclojure.lang.PersistentVector;")
  [a]
  "1d Persistent Vector")
;; => #multifn[what-is 0x440212f4]

(asum-sq (double-array [1 2 3 4 5]))
;; => 55.0

(what-is (into-array ["a" "b"]))
;; => "1d String"
;; => 

(what-is (to-array-2d [[1 2] [3 4]]))
;; => "2d Object"

(what-is (make-array Integer/TYPE 2 2 2 2))
;; => "Primitive 4d int"

(what-is (into-array (map double-array [[1.0] [2.0]])))
;; => "Primitive 2d double"

(what-is (into-array [[1.0] [2.0]]))
;; => "1d Persistent Vector"

(String/format "An int %d and a String %s"
               (to-array [99, "luftballons"]))
;; => "An int 99 and a String luftballons"

(defn gimme
  []
  (ArrayList. [1 3 4 8 2]))
;; => #'ch12-java.core/gimme

(doto (gimme)
  (Collections/sort (Collections/reverseOrder)))
;; => [8 4 3 2 1]

(doto (Thread. #(do (Thread/sleep 5000)
                    (println "hehehee")))
  .start)
;; => #object[java.lang.Thread 0x346f4c84 "Thread[Thread-35,5,main]"]
;; ... 5 s later
;; => hehehee

(let [f (FutureTask. #(do (Thread/sleep 5000) 42))]
  (.start (Thread. #(.run f)))
  (.get f))
;; => 42

;;;;;;;;;; java.util.List interface
(.get '[a b c] 1)
;; => b

(.get (repeat :a) 138)
;; => :a

(.containsAll '[a b c] '[a b])
;; => true

(.add '[a b c] 'd)
;; => Execution error (UnsupportedOperationException)

;;;;;;;;;;; java.lang.Comparable interface
(.compareTo [:a] [:a])
;; => 0

(.compareTo [:a :b] [:a])
;; => 1

(.compareTo [:a :b] [:a :b :c])
;; => -1

(sort [[:a :b :c] [:a] [:a :b]])
;; => ([:a] [:a :b] [:a :b :c])

;;;;;;;;;;;; java.util.RandomAccess interface
(.get '[a b c] 2)
;; => c

;;;;;;;;;;; java.util.Collections interface
(defn shuffle'
  [coll]
  (seq (doto (java.util.ArrayList. coll)
         java.util.Collections/shuffle)))
;; => #'ch12-java.core/shuffle'

(shuffle' (range 10))
;; => (6 9 5 8 4 2 1 3 0 7)

;;;;;;;;;;; java.util.Map interface
(java.util.Collections/unmodifiableMap
 (doto (java.util.HashMap.) (.put :a 1)))
;; => {:a 1}

(into {} (doto (java.util.HashMap.) (.put :a 1)))
;; => {:a 1}

(def x (java.awt.Point. 0 0))
(def y (java.awt.Point. 0 42))
(def points #{x y})
points
;; => #{#object[java.awt.Point 0x2827fb5f "java.awt.Point[x=0,y=0]"] #object[java.awt.Point 0x4eb975b2 "java.awt.Point[x=0,y=42]"]}

(.setLocation y 0 0)
points
;; => #{#object[java.awt.Point 0x2827fb5f "java.awt.Point[x=0,y=0]"] #object[java.awt.Point 0x4eb975b2 "java.awt.Point[x=0,y=0]"]}

;;;;;;; interface ;;;;;;
(definterface ISliceable
  (slice [^long s ^long e])
  (^long sliceCount []))
;; => ch12_java.core.ISliceable

(def dumb
  (reify ch12_java.core.ISliceable
    (slice [_ s e] [:empty])
    (sliceCount [_] 42)))
;; => #'ch12-java.core/dumb

(.slice dumb 1 2)
;; => [:empty]

(.sliceCount dumb)
;; => 42

(defprotocol Sliceable
  (slice [this s e])
  (sliceCount [this]))
;; => Sliceable

(extend ch12_java.core.ISliceable
  Sliceable
  {:slice (fn [this s e] (.slice this s e))
   :sliceCount (fn [this] (.sliceCount this))})
;; => nil

(sliceCount dumb)
;; => 42

(slice dumb 0 0)
;; => [:empty]

(defn calc-slice-count
  [thing]
  "Calculates the number of possible slices using the formula:
(n + r - 1)!
------------
 r! (n - 1)!
where n is (count thing) and r is 2"
  (let [! #(reduce * (take % (iterate inc 1)))
        n (count thing)]
    (/ (! (- (+ n 2) 1))
       (* (! 2) (! (- n 1))))))
;; => #'ch12-java.core/calc-slice-count

(extend-type String
  Sliceable
  (slice [this s e] (.substring this s (inc e)))
  (sliceCount [this] (calc-slice-count this)))
;; => nil

(slice "abc" 0 1)
;; => "ab"

(sliceCount "abc")
;; => 6

(defmacro pairs
  [& args]
  (if (even? (count args))
    `(partition 2 '~args)
    (throw (Exception. (str "pairs requires an even number of args")))))
;; => #'ch12-java.core/pairs

(pairs 1 2 3)
;; => Syntax error macroexpanding pairs at (src/ch12_java/core.clj:379:1).
;; => pairs requires an even number of args

(pairs 1 2 3 4)
;; => ((1 2) (3 4))

(defmacro -?>
  [& forms]
  `(try (-> ~@forms)
        (catch NullPointerException _# nil)))
;; => #'ch12-java.core/-?>

(-?> 25 Math/sqrt (+ 100))
;; => 105.0

(-?> 25 Math/sqrt (and nil) (+ 100))
;; => nil

(defn perform-unclean-act
  [x y]
  (/ x y))
;; => #'ch12-java.core/perform-unclean-act

(try
  (perform-unclean-act 42 0)
  (catch RuntimeException ex
    (println (str "something went wrong"))))
;; => nil
;; something went wrong

(defn perform-cleaner-act
  [x y]
  (try
    (/ x y)
    (catch ArithmeticException ex
      (throw (ex-info "You attempted an unclean act"
                      {:args [x y]})))))
;; => #'ch12-java.core/perform-cleaner-act

(try
  (perform-cleaner-act 108 0)
  (catch RuntimeException ex
    (println (str "Received error: " (.getMessage ex)))
    (when-let [ctx (ex-data ex)]
      (println (str "More information: " ctx)))))
;; => nil
;; Received error: You attempted an unclean act
;; More information: {:args [108 0]}
