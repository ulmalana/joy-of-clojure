(ns ch09-combine-data-code.udp
  (:refer-clojure :exclude [get, cat]))

(defn beget
  [this proto]
  (assoc this ::prototype proto))


(beget {:sub 0} {:super 1})
;; => {:sub 0, :ch09-combine-data-code.udp/prototype {:super 1}}

(defn get
  [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(get (beget {:sub 0} {:super 1})
     :super)
;; => 1

(def put assoc)

(def cat {:likes-dogs true, :ocd-bathing true})
(def morris (beget {:likes-9lives true} cat))
(def post-traumatic-morris (beget {:likes-dogs nil} morris))

cat

morris

post-traumatic-morris

(get cat :likes-dogs)
;; => true

(get morris :likes-dogs)
;; => true

(get post-traumatic-morris :likes-dogs)
;; => nil

(get post-traumatic-morris :likes-9lives)
;; => true
