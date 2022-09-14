(ns ch10-mutation-concurrency.locks
  (:refer-clojure :exclude [aget aset count seq])
  (:require [clojure.core :as clj])
  (:use [ch10-mutation-concurrency.core :only (dothreads!)])
  (:import (java.util.concurrent.locks ReentrantLock)))

(defprotocol SafeArray
  (aset [this i f])
  (aget [this i])
  (count [this])
  (seq [this]))

(defn make-dumb-array
  [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i] (clj/aget a i))
      (aset [this i f]
        (clj/aset a i (f (aget this i)))))))

(defn pummel
  [a]
  (dothreads! #(dotimes [i (count a)] (aset a i inc))
              :threads 100))

(def D (make-dumb-array Integer/TYPE 8))

(pummel D)
;; => nil

(seq D)
;; => (93 94 94 95 95 70 97 96) ***Note: should have 100 in each slot***

(defn make-safe-array
  [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (locking a
          (clj/aget a i)))
      (aset [this i f]
        (locking a
          (clj/aset a i (f (aget this i))))))))

(def A (make-safe-array Integer/TYPE 8))

(pummel A)
;; => nil

(seq A)
;; => (100 100 100 100 100 100 100 100)

(defn lock-i
  [target-index num-locks]
  (mod target-index num-locks))

(defn make-smart-array
  [t sz]
  (let [a (make-array t sz)
        Lsz (/ sz 2)
        L (into-array (take Lsz
                            (repeatedly #(ReentrantLock.))))]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aget a i)
            (finally (.unlock lk)))))
      (aset [this i f]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aset a i (f (aget this i)))
            (finally (.unlock lk))))))))

(def S (make-smart-array Integer/TYPE 8))

(pummel S)
;; => nil

(seq S)
;; => (100 100 100 100 100 100 100 100)
