(ns ch09-combine-data-code.ns-ex
  (:refer-clojure :exclude [defstruct])
  (:use (clojure set xml))
  (:use [clojure.test :only (are is)])
  (:require (clojure [zip :as z]))
  (:import (java.util Date)
           (java.io File)))

(defn ^{:private true} ns-ex-answer [] 42)

(defn ns-ex-print
  []
  (println "This is from nx-ex namespace"))
