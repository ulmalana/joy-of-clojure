(ns ch10-mutation-concurrency.core
  (:import java.util.concurrent.Executors)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;;;;; dothreads ;;;;;;;;;;;;;;;

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

(dothreads! #(.print System/out "Hi ") :threads 2 :times 2)
;; => nil
;;;;;;;;;;;;;;;;;;;;; refs ;;;;;;;;;;;;;;;;;;;;;
(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map
  [f board]
  (vec (map
        #(vec (for [s %] (f s)))
        board)))

;; mutable state
(defn reset-board!
  "Resets the board state. Generally these types of functions are bad idea."
  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %))
                deltas))))
(def king-moves
  (partial neighbors
           [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]] 3))

(defn good-move?
  [to enemy-sq]
  (when (not= to enemy-sq)
    to))

(defn choose-move
  "Randomly choose a legal move"
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(reset-board!)
(take 5 (repeatedly #(choose-move @to-move)))
;; => ([:K [2 2]] [:K [2 0]] [:K [2 2]] [:K [1 2]] [:K [1 2]])

(defn place [from to] to)

(defn move-piece
  [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src) place :-)
  (alter num-moves inc))

(defn update-to-move
  [move]
  (alter to-move #(vector (second %) move)))

(defn make-move
  []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

(reset-board!)

(make-move)
;; => [[:k [0 1]] [:K [2 0]]]

(board-map deref board)
;; => [[:- :k :-] [:- :- :-] [:K :- :-]]

(make-move)
;; => [[:K [2 0]] [:k [1 1]]]

(board-map deref board)
;; => [[:- :- :-] [:- :k :-] [:K :- :-]]

(dothreads! make-move :threads 100 :times 100)

(board-map deref board)
;; => [[:- :- :-] [:- :k :-] [:k :- :-]]

;;;;;;;;;;;;; refs ;;;;;;;;;;;;;;;;;

(defn make-move-v2 []
  (dosync
   (let [move (choose-move @to-move)]
     (move-piece move @to-move)
     (update-to-move move))))

(reset-board!)
;; => #'ch10-mutation-concurrency.core/num-moves
(make-move)
;; => [[:k [0 1]] [:K [1 1]]]

(board-map deref board)
;; => [[:- :k :-] [:- :K :-] [:- :- :-]]

@num-moves
;; => 1

(dothreads! make-move-v2 :threads 100 :times 100)
;; => nil

(board-map #(dosync (deref %)) board)
;; => [[:- :- :-] [:- :- :k] [:- :- :K]]

@to-move

@num-moves
;; => 10001
;; => 10001

(defn stress-ref
  [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
       (swap! slow-tries inc)
       (Thread/sleep 200)
       @r)
      (println (format "r is: %s, history: %d, after %d tries" @r (.getHistoryCount r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

(stress-ref (ref 0))
;; => :done
;; => r is: 500, history: 10, adter 27 tries

(stress-ref (ref 0 :max-history 30))
;; => :done
;; => r is: 407, history: 20, after 21 tries

(stress-ref (ref 0 :min-history 15 :max-history 30))
;; => r is: 116, history: 20, after 6 tries
;; => :done

;;;;;;;;;;;;;;;;;;;;; agents ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def joy (agent []))


(send joy conj "First edition")
;; => #agent[{:status :ready, :val ["First edition"]} 0x5a94ee47]

@joy
;; => ["First edition"]

(defn slow-conj
  [coll item]
  (Thread/sleep 1000)
  (conj coll item))

(send joy slow-conj "Second edition")
;; => #agent[{:status :ready, :val ["First edition"]} 0x7c084410]

@joy
;; => ["First edition" "Second edition"]

(def log-agent (agent 0))

(defn do-log
  [msg-id message]
  (println msg-id ":" message)
  (inc msg-id))

(defn do-step
  [channel message]
  (Thread/sleep 1)
  (send-off log-agent do-log (str channel message)))

(defn three-step
  [channel]
  (do-step channel " ready to begin (step 0)")
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting goinf now (step 2)")
  (do-step channel " don! (step 3)"))

(defn all-together-now
  []
  (dothreads! #(three-step "alpha"))
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "omega")))

(all-together-now)
;; 0 : omega ready to begin (step 0)
;; 1 : alpha ready to begin (step 0)
;; 2 : beta ready to begin (step 0)
;; 3 : omega warming up (step 1)
;; 4 : omega really getting goinf now (step 2)
;; 5 : alpha warming up (step 1)
;; 6 : beta warming up (step 1)
;; 7 : omega don! (step 3)
;; 8 : beta really getting goinf now (step 2)
;; 9 : alpha really getting goinf now (step 2)
;; 10 : beta don! (step 3)
;; 11 : alpha don! (step 3)

@log-agent
;; => 12

(do-step "important: " "this must go out")
;; => #agent[{:status :ready, :val 13} 0x7fc5efc]
(await log-agent)
;; => nil

(send log-agent (fn [_] 1000))
;; => #agent[{:status :ready, :val 1000} 0x7fc5efc]

(do-step "epsilon " "near miss")
;; => #agent[{:status :ready, :val 1001} 0x7fc5efc]

(defn exercise-agents
  [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))
;; => #'ch10-mutation-concurrency.core/exercise-agents

(time (exercise-agents send-off))
;; "Elapsed time: 1003.575077 msecs"

(time (exercise-agents send))
;; "Elapsed time: 2003.277304 msecs"

(defn handle-log-error
  [the-agent the-err]
  (println "An action sent to the log-agent threw " the-err))

;;;;;;;;;;;;;;;;;;;;;;;;; atoms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *time* (atom 0))

(defn tick
  []
  (swap! *time* inc))

(dothreads! tick :threads 1000 :times 100)

@*time*
;; => 100000

(defn manipulable-memoize
  [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 1000) x))

(time [(slowly 9) (slowly 9)])
;; => "Elapsed time: 2006.025298 msecs"
;; => [9 9]

(def sometimes-slowly (manipulable-memoize slowly))
;; => #'ch10-mutation-concurrency.core/sometimes-slowly

(time [(sometimes-slowly 108) (sometimes-slowly 108)])
;; => "Elapsed time: 1007.87787 msecs"
;; => [108 108]

(meta sometimes-slowly)
;; => {:cache #atom[{(108) 108} 0x56320c8e]}

(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(108)))
;; => {}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*read-eval*

(var *read-eval*)
;; => #'clojure.core/*read-eval*

(defn print-read-eval
  []
  (println "*read-eval* is currently" *read-eval*))

(defn binding-play
  []
  (print-read-eval)
  (binding [*read-eval* false]
    (print-read-eval))
  (print-read-eval))

(binding-play)
;; *read-eval* is currently true
;; *read-eval* is currently false
;; *read-eval* is currently true

(def x 55)
;; => #'ch10-mutation-concurrency.core/x
{:outer-var-value x
 :with-locals (with-local-vars [x 9]
                {:local-var x
                 :local-var-value (var-get x)})}
;; => {:outer-var-value 55, :with-locals {:local-var #<Var: --unnamed-->, :local-var-value 9}}
