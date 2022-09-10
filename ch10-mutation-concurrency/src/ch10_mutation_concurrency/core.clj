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
