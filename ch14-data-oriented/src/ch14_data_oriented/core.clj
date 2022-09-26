(ns ch14-data-oriented.core
  (:use clojure.data)
  (:require [clojure.edn :as edn]
            [clojure.set :as sql])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def ascii (map char (range 65 (+ 65 26))))

(defn rand-str
  [size alphabet]
  (apply str (repeatedly size #(rand-nth alphabet))))

(rand-str 10 ascii)
;; => "LQUGLOMEDK"

(def rand-sym #(symbol (rand-str %1 %2)))

(def rand-key #(keyword (rand-str %1 %2)))

(rand-key 10 ascii)
;; => :SQIUOZDUGP

(rand-sym 10 ascii)
;; => NQIDGSKEGU

(defn rand-vec
  [& generators]
  (into [] (map #(%) generators)))

(rand-vec #(rand-sym 5 ascii)
          #(rand-key 10 ascii)
          #(rand-int 1024))
;; => [PABFI :CWFJUEQNCC 239]

(defn rand-map
  [size kgen vgen]
  (into {}
        (repeatedly size #(rand-vec kgen vgen))))

(rand-map 3 #(rand-key 5 ascii) #(rand-int 100))
;; => {:OFXOJ 16, :UROIH 78, :SJRVB 33}

(assert (= [1 2 3] (conj [1 2] 3)))
;; => nil

(diff [1 2 3] [1 2 4])
;; => [[nil nil 3] [nil nil 4] [1 2]]

(defn filter-rising
  [segments]
  (clojure.set/select
   (fn [{:keys [p1 p2]}]
     (> 0
        (/ (- (p2 0) (p1 0))
           (- (p2 1) (p1 1)))))
   segments))

(filter-rising #{{:p1 [0 0] :p2 [1 1]}
                 {:p1 [4 15] :p2 [3 21]}})
;; => #{{:p1 [4 15], :p2 [3 21]}}

(edn/read-string "#uuid \"dae78a90-d491-11e2-8b8b-0800200c9a66\"")
;; => #uuid "dae78a90-d491-11e2-8b8b-0800200c9a66"

(edn/read-string "42")
;; => 42

(edn/read-string "{:a 42 \"b\" 36 [:c] 9}")
;; => {:a 42, "b" 36, [:c] 9}


(defn valid?
  [event]
  (boolean (:result event)))

(valid? {})
;; => false

(valid? {:result 42})
;; => true

(defn effect
  [{:keys [ab h] :or {ab 0, h 0}}
   event]
  (let [ab (inc ab)
        h (if (= :hit (:result event))
            (inc h)
            h)
        avg (double (/ h ab))]
    {:ab ab :h h :avg avg}))

(effect {} {:result :hit})
;; => {:ab 1, :h 1, :avg 1.0}

(effect {:ab 599 :h 180}
        {:result :out})
;; => {:ab 600, :h 180, :avg 0.3}

(defn apply-effect
  [state event]
  (if (valid? event)
    (effect state event)
    state))

(apply-effect {:ab 600 :h 180 :avg 0.3}
              {:result :hit})
;; => {:ab 601, :h 181, :avg 0.3011647254575707}

(def effect-all #(reduce apply-effect %1 %2))

(effect-all {:ab 0 :h 0}
            [{:result :hit}
             {:result :out}
             {:result :hit}
             {:result :out}])
;; => {:ab 4, :h 2, :avg 0.5}

(def events (repeatedly 100
                        (fn []
                          (rand-map 1
                                    #(-> :result)
                                    #(if (< (rand-int 10) 3)
                                       :hit
                                       :out)))))

(effect-all {} events)
;; => {:ab 100, :h 29, :avg 0.29}

(effect-all {} events)
;; => {:ab 100, :h 29, :avg 0.29}

(effect-all {} (take 50 events))
;; => {:ab 50, :h 17, :avg 0.34}

(def fx-timeline #(reductions apply-effect %1 %2))

(fx-timeline {} (take 3 events))
;; => ({} {:ab 1, :h 1, :avg 1.0} {:ab 2, :h 2, :avg 1.0} {:ab 3, :h 3, :avg 1.0})


(def PLAYERS #{{:player "Nick" :ability 32/100}
               {:player "Matt" :ability 26/100}
               {:player "Ryan" :ability 19/100}})

(defn lookup
  [db name]
  (first (sql/select
          #(= name (:player %))
          db)))
(lookup PLAYERS "Nick")
;; => {:player "Nick", :ability 32}

(defn update-stats
  [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db
          (merge player (effect player event)))))

(update-stats PLAYERS {:player "Nick" :result :hit})
;; => #{{:player "Matt", :ability 26} {:player "Ryan", :ability 19} {:player "Nick", :ability 32, :ab 1, :h 1, :avg 1.0}}

(defn commit-event
  [db event]
  (dosync (alter db update-stats event)))

(commit-event (ref PLAYERS) {:player "Nick" :result :hit})
;; => #{{:player "Matt", :ability 26} {:player "Ryan", :ability 19} {:player "Nick", :ability 32, :ab 1, :h 1, :avg 1.0}}

(defn rand-event
  [{ability :ability}]
  (let [able (numerator ability)
        max (denominator ability)]
    (rand-map 1
              #(-> :result)
              #(if (< (rand-int max) able)
                 :hit
                 :out))))

(defn rand-events
  [total player]
  (take total
        (repeatedly #(assoc (rand-event player)
                            :player
                            (:player player)))))

(rand-events 3 {:player "Nick" :ability 32/100})
;; => ({:result :hit, :player "Nick"} {:result :hit, :player "Nick"} {:result :hit, :player "Nick"})

(def agent-for-player
  (memoize
   (fn [player-name]
     (-> (agent [])
         (set-error-handler! #(println "Error: " %1 %2))
         (set-error-mode! :fail)))))

(defn feed
  [db event]
  (let [a (agent-for-player (:player event))]
    (send a
          (fn [state]
            (commit-event db event)
            (conj state event)))))

(defn feed-all
  [db events]
  (doseq [event events]
    (feed db event))
  db)

;; (let [db (ref PLAYERS)]
;;   (feed-all db (rand-events 100 {:player "Nick", :ability 32/100}))
;;   db)
;; => 
;; => 

(defn simulate
  [total players]
  (let [events (apply interleave
                      (for [player players]
                        (rand-events total player)))
        results (feed-all (ref players) events)]
    (apply await (map #(agent-for-player (:player %)) players))
    @results))

;; (simulate 2 PLAYERS)
;; ;; => 
;; (simulate 400 PLAYERS)

(defn meters->feet [m] (* m 3.28083989501312))
(defn meters->miles [m] (* m 0.000621))

(meters->feet 1609.344)
;; => 5279.9999999999945

(meters->miles 1609.344)
;; => 0.999402624

(defn convert
  [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(defn relative-units
  [context unit]
  (if-let [spec (get context unit)]
    (if (vector? spec)
      (convert context spec)
      spec)
    (throw (RuntimeException. (str "undefined unit " unit)))))

(relative-units {:m 1 :cm 1/100 :mm [1/10 :cm]} :m)
;; => 1

(defmacro defunits-of
  [name base-unit & conversions]
  (let [magnitude (gensym)
        unit (gensym)
        units-map (into `{~base-unit 1}
                        (map vec (partition 2 conversions)))]
    `(defmacro ~(symbol (str "unit-of-" name))
       [~magnitude ~unit]
       `(* ~~magnitude
           ~(case ~unit
              ~@(mapcat
                 (fn [[u# & r#]]
                   `[~u# ~(relative-units units-map u#)])
                 units-map))))))

(defunits-of distance :m
  :km 1000
  :cm 1/100
  :mm [1/10 :cm]
  :ft 0.3048
  :mile [5280 :ft])

(unit-of-distance 1 :m)
;; => 1

(unit-of-distance 1 :mm)
;; => 1/1000

(unit-of-distance 1 :ft)
;; => 0.3048

(unit-of-distance 1 :mile)
;; => 1609.344
