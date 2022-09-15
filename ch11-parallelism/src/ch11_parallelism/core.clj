(ns ch11-parallelism.core
  (:require (clojure [xml :as xml]))
  (:require (clojure [zip :as zip]))
  (:require (clojure.core [reducers :as r]))
  (:import (java.util.regex Pattern)
           (java.util.concurrent Executors))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;;;;;;;;;;;;;;;; futures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (time (let [x (future (do (Thread/sleep 5000) (+ 41 1)))]
;;          [@x @x]))
;; ;; "Elapsed time: 5002.2647 msecs" ****Note: only sleeps once*****
;; => [42 42]

(defn feed->zipper
  [uri-str]
  (->> (xml/parse uri-str)
       zip/xml-zip))

(defn normalize
  [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children
  [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag))))

(defn title
  [entry]
  (some->> entry
           :content
           (some #(when (= :title (:tag %)) %))
           :content
           first))

(defn count-text-task
  [extractor txt feed]
  (let [items (feed-children feed)
        re (Pattern/compile (str "(?i)" txt))]
    (->> items
         (map extractor)
         (mapcat #(re-seq re %))
         count)))

(count-text-task
 title
 "Erlang"
 "http://feeds.feedburner.com/ElixirLang")
;; => 0
;; => 0

(count-text-task
 title
 "Elixir"
 "http://feeds.feedburner.com/ElixirLang")
;; => 47

(def feeds #{"http://feeds.feedburner.com/ElixirLang"
             "http://blog.fogus.me/feed/"})
;; => #'ch11-parallelism.core/feeds

(let [results (for [feed feeds]
                (future
                  (count-text-task title "Elixir" feed)))]
  (reduce + (map deref results)))
;; => 47

(defmacro as-futures
  [[a args] & body]
  (let [parts (partition-by #{'=>} body)
        [acts _ [res]] (partition-by #{:as} (first parts))
        [_ _ task] parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))
;; => #'ch11-parallelism.core/as-futures

;; how to use as-futures
;; (as-futures [<arg-name> <all-args>]
;;             <actions-using-args>
;;             :as <results-name>
;;             =>
;;             <actions-using-results>)

(defn occurrences
  [extractor tag & feeds]
  (as-futures [feed feeds]
              (count-text-task extractor tag feed)
              :as results
              =>
              (reduce + (map deref results))))
;; => #'ch11-parallelism.core/occurrences

(occurrences title "released"
             "http://blog.fogus.me/feed"
             "http://feeds.feedburner.com/ElixirLang"
             "http://www.ruby-lang.org/en/feeds/news.rss")
;; => 37

;;;;;;;;;;;;;;;;;;;;;;; promises ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def thread-pool
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(def x (promise))
(def y (promise))
(def z (promise))

(dothreads! #(deliver z (+ @x @y)))

(dothreads!
 #(do (Thread/sleep 2000) (deliver x 52)))

(dothreads!
 #(do (Thread/sleep 4000) (deliver y 86)))

(time @z)
;; "Elapsed time: 3998.84226 msecs"
;; => 138

(defmacro with-promises
  [[n tasks _ as] & body]
  (when as
    `(let [tasks# ~tasks
           n# (count tasks#)
           promises# (take n# (repeatedly promise))]
       (dotimes [i# n#]
         (dothreads!
          (fn []
            (deliver (nth promises# i#)
                     ((nth tasks# i#))))))
       (let [~n tasks#
             ~as promises#]
         ~@body))))

(defrecord TestRun [run passed failed])

(defn pass [] true)
(defn fail [] false)

(defn run-tests
  [& all-tests]
  (with-promises
    [tests all-tests :as results]
    (into (TestRun. 0 0 0)
          (reduce #(merge-with + %1 %2) {}
                  (for [r results]
                    (if @r
                      {:run 1 :passed 1}
                      {:run 1 :failed 1}))))))

(run-tests pass fail fail fail pass)
;; => #ch11_parallelism.core.TestRun{:run 5, :passed 2, :failed 3}

(defn feed-items
  [k feed]
  (k
   (for [item (filter (comp #{:entry :item} :tag)
                      (feed-children feed))]
     (-> item :content first :content))))

(feed-items
 count
 "http://blog.fogus.me/feed/")
;; => 5

(let [p (promise)]
  (feed-items #(deliver p (count %))
              "http://blog.fogus.me/feed/")
  @p)
;; => 5

(defn cps->fn
  [f k]
  (fn [& args]
    (let [p (promise)]
      (apply f (fn [x] (deliver p (k x))) args)
      @p)))
;; => #'ch11-parallelism.core/cps->fn

(def count-items (cps->fn feed-items count))
;; => #'ch11-parallelism.core/count-items

(count-items "http://blog.fogus.me/feed/")
;; => 5

;;;; pvalues ;;;;;;;
;; executes abitrary number of expression in parallel
;; returns : lazy seq
(pvalues 1 2 (+ 1 2))
;; => (1 2 3)

(defn sleeper
  [s thing]
  (Thread/sleep (* 1000 s)) thing)
;; => #'ch11-parallelism.core/sleeper

(defn pvs
  []
  (pvalues
   (sleeper 2 :1st)
   (sleeper 3 :2nd)
   (keyword "3rd")))
;; => #'ch11-parallelism.core/pvs

(-> (pvs) first time)
;; "Elapsed time: 2004.467868 msecs"
;; => :1st

(-> (pvs) first time)
;; => :1st

(-> (pvs) last time)
;; "Elapsed time: 3002.079701 msecs"
;; => :3rd

;;;;;;;;;; pmap ;;;;;;;;;;;;;;
;; parallel version of map
(->> [1 2 3]
     (pmap (comp inc (partial sleeper 2)))
     doall
     time)
;; "Elapsed time: 2001.539888 msecs"
;; => (2 3 4)

;;;;;;;; pcalls ;;;;;;;;;;;;;;;;;;;
;; takes arbitrary number of functions (taking no arguments)
;; call them in parallel, then return lazy seq of the results
(-> (pcalls
     #(sleeper 2 :first)
     #(sleeper 3 :second)
     #(keyword "3rd"))
    doall
    time)
;; "Elapsed time: 3003.776687 msecs"
;; => (:first :second :3rd)

;;;;;;;;;;;;;;;;;;;;;;;;;;; fold ;;;;;;;;;;;;;;;;;;;
(def big-vec (vec (range (* 1000 1000))))
;; => #'ch11-parallelism.core/big-vec

(time (reduce + big-vec))
;; "Elapsed time: 51.034896 msecs"
;; => 499999500000

(time (r/fold + big-vec))
;; "Elapsed time: 43.993545 msecs"
;; => 499999500000
