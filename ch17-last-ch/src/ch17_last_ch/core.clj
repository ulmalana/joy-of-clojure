(ns ch17-last-ch.core
  (:require [clojure.set :as ra]
            [clojure [xml :as xml]]
            [clojure [zip :as zip]]
            [clojure.test :refer (deftest testing is)])
  (:use [clojure.string :as str :only []])
  (:import (java.util.regex Pattern))
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

(defn feed->zipper [uri-str]
  (->> (xml/parse uri-str)
       zip/xml-zip))

(defn normalize [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag))))

(def stubbed-feed-children
  (constantly [{:content [{:tag :title
                           :content ["Stub"]}]}]))

(defn count-feed-entries [url]
  (count (feed-children url)))

(count-feed-entries "http://blog.fogus.me/feed/")
;; => 5

(with-redefs [feed-children stubbed-feed-children]
  (count-feed-entries "dummy url"))
;; => 1

(defn title [entry]
  (some->> entry
           :content
           (some #(when (= :title (:tag %)) %))
           :content
           first))

(defn count-text-task [extractor txt feed]
  (let [items (feed-children feed)
        re (Pattern/compile (str "(?i)" txt))]
    (->> items
         (map extractor)
         (mapcat #(re-seq re %))
         count)))

(deftest feed-tests
  (with-redefs [feed-children stubbed-feed-children]
    (testing "Child counting"
      (is (= 1000 (count-feed-entries "Dummy url"))))
    (testing "Ocurrence counting"
      (is (= 0 count-text-task
             title
             "ZOMG"
             "Dummy url")))))

(clojure.test/run-tests 'ch17-last-ch.core)

;; Testing ch17-last-ch.core

;; FAIL in (feed-tests) (core.clj:180)
;; Child counting
;; expected: (= 1000 (count-feed-entries "Dummy url"))
;; actual: (not (= 1000 1))

;; FAIL in (feed-tests) (core.clj:182)
;; Ocurrence counting
;; expected: (= 0 count-text-task title "ZOMG" "Dummy url")
;; actual: (not (= 0 #function[ch17-last-ch.core/count-text-task] #function[ch17-last-ch.core/title] "ZOMG" "Dummy url"))

;; Ran 1 tests containing 2 assertions.
;; 2 failures, 0 errors.
;; {:test 1, :pass 0, :fail 2, :error 0, :type :summary}


(defmacro defformula [nm bindings & formula]
  `(let ~bindings
     (let [formula# (agent ~@formula)
           update-fn# (fn [key# ref# o# n#]
                        (send formula# (fn [_#] ~@formula)))]
       (doseq [r# ~(vec (map bindings
                             (range 0 (count bindings) 2)))]
         (add-watch r# :update-formula update-fn#))
       (def ~nm formula#))))

(def h (ref 25))
(def ab (ref 100))

(defformula avg
  [at-bats ab, hits h]
  (float (/ @hits @at-bats)))

@avg
;; => 0.25

(dosync (ref-set h 33))
;; => 33

@avg
;; => 0.33

(def config
  '{:systems {:pump {:type :feeder, :descr "Feeder system"}
              :sim1 {:type :sim, :fidelity :low}
              :sim2 {:type :sim, :fidelity :high, :threads 2}}})

(defn describe-system [name cfg]
  [(:type cfg) (:fidelity cfg)])

(describe-system :pump {:type :feeder, :descr "Feeder system"})
;; => [:feeder nil]

(defmulti construct describe-system)

(defmethod construct :default [name cfg]
  {:name name
   :type (:type cfg)})

(defn construct-subsystem [sys-map]
  (for [[name cfg] sys-map]
    (construct name cfg)))

(construct-subsystem (:systems config))
;; => ({:name :pump, :type :feeder} {:name :sim1, :type :sim} {:name :sim2, :type :sim})

(defmethod construct [:feeder nil]
  [_ cfg]
  (:descr cfg))

(construct-subsystem (:systems config))
;; => ("Feeder system" {:name :sim1, :type :sim} {:name :sim2, :type :sim})

(defrecord LowFiSim [name])
(defrecord HiFiSim [name threads])

(defmethod construct [:sim :low]
  [name cfg]
  (->LowFiSim name))

(defmethod construct [:sim :high]
  [name cfg]
  (->HiFiSim name (:threads cfg)))

(construct-subsystem (:systems config))
;; => ("Feeder system" #ch17_last_ch.core.LowFiSim{:name :sim1} #ch17_last_ch.core.HiFiSim{:name :sim2, :threads 2})

(def lofi {:type :sim, :descr "Lowfi sim", :fidelity :low})
(def hifi {:type :sim, :descr "Hifi sim", :fidelity :high, :threads 2})

(construct :lofi lofi)
;; => #ch17_last_ch.core.LowFiSim{:name :lofi}

(defprotocol Sys
  (start! [sys])
  (stop! [sys]))

(defprotocol Sim
  (handle [sim msg]))

(defn build-system [name config]
  (let [sys (construct name config)]
    (start! sys)
    sys))

(extend-type LowFiSim
  Sys
  (start! [this]
    (println "Started a lofi simulator"))
  (stop! [this]
    (println "Stopped a lofi simulator"))
  Sim
  (handle [this msg]
    (* (:weight msg) 3.14)))

(start! (construct :lofi lofi))
;; Started a lofi simulator
;; => nil

(build-system :sim1 lofi)
;; Started a lofi simulator
;; => #ch17_last_ch.core.LowFiSim{:name :sim1}

(handle (build-system :sim1 lofi) {:weight 42})
;; => 131.88

(extend-type HiFiSim
  Sys
  (start! [this] (println "Started a hifi simulator"))
  (stop! [this] (println "stopped a hifi simulator"))

  Sim
  (handle [this msg]
    (Thread/sleep 5000)
    (* (:weight msg) Math/PI)))

(build-system :sim2 hifi)
;; => #ch17_last_ch.core.HiFiSim{:name :sim2, :threads 2}

(handle (build-system :sim2 hifi) {:weight 42})
;; => 131.94689145077132

(def excellent (promise))

(defn simulate [answer fast slow opts]
  (future (deliver answer (handle slow opts)))
  (handle fast opts))

(simulate excellent
          (build-system :sim1 lofi)
          (build-system :sim2 hifi)
          {:weight 42})
;; => 131.88

(defrecord MockSim [name])

(def starts (atom 0))

(extend-type MockSim
  Sys
  (start! [this]
    (if (= 1 (swap! starts inc))
      (println "Started a mock simulator")
      (throw (RuntimeException. "Called start! more than once"))))
  (stop! [this] (println "Stopped a mock simulator"))
  Sim
  (handle [_ _] 42))

(defmethod construct [:mock nil]
  [nom _]
  (MockSim. nom))

(defn initialize [name cfg]
  (let [lib (:lib cfg)]
    (require lib)
    (build-system name cfg)))

(defn traverse [node f]
  (when node
    (f node)
    (doseq [child (:content node)]
      (traverse child f))))

(def DB
  (-> "<zoo>
         <pongo>
            <animal>orangutan</animal>
         </pongo>
         <panthera>
            <animal>Spot</animal>
            <animal>lion</animal>
            <animal>Lopshire</animal>
         </panthera>
       </zoo>"
      .getBytes
      (java.io.ByteArrayInputStream.)
      xml/parse))

(defn ^:dynamic handle-weird-animal
  [{[name] :content}]
  (throw (Exception. (str name " must be a 'dealt with'"))))

(defmulti visit :tag)

(defmethod visit :animal [{[name] :content :as animal}]
  (case name
    "Spot" (handle-weird-animal animal)
    "Lopshire" (handle-weird-animal animal)
    (println name)))

;; (traverse DB visit)
;; => 

(defn readr [prompt exit-code]
  (let [input (clojure.main/repl-read prompt exit-code)]
    (if (= input ::tl)
      exit-code
      input)))

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym))
                 symbols)
            symbols)))

(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(defmacro break []
  `(clojure.main/repl
    :prompt #(print "debug=> ")
    :read readr
    :eval (partial contextual-eval (local-context))))

(defn keys-apply [f ks m]
  (break)
  (let [only (select-keys m ks)]
    (break)
    (zipmap (keys only) (map f (vals only)))))

(defmacro awhen [expr & body]
  (break)
  `(let [~'it ~expr]
     (if ~'it
       (do (break) ~@body))))
