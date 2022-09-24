(ns ch13-clojurescript.core
  (:require [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [clojure.walk :refer [prewalk]]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io])
  (:import (clojure.lang LineNumberingPushbackReader))
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def code-string "(defn hello [x] (js/alert (pr-str 'greetings x)))")

(def code-data (read-string code-string))

code-data
;; => (defn hello [x] (js/alert (pr-str (quote greetings) x)))

(first code-string)
;; => \(

(first code-data);; => defn

(def ast (ana/analyze (ana/empty-env) code-data))

(keys ast)
;; => (:children :init :name :op :env :var :form :tag :doc :jsdoc)

(defn print-ast
  [ast]
  (pprint
   (prewalk
    (fn [x]
      (if (map? x)
        (select-keys x [:children :name :form :op])
        x))
    ast)))
;; => #'ch13-clojurescript.core/print-ast

(print-ast ast)
;; {:children
;;  [{:children
;;    [{:children
;;      [{:children
;;        [{:form js/alert, :op :var}
;;         {:children
;;          [{:form pr-str, :op :var}
;;           {:form greetings, :op :const}
;;           {:form x, :op :var}],
;;          :form (pr-str 'greetings x),
;;          :op :invoke}],
;;        :form (js/alert (pr-str 'greetings x)),
;;        :op :invoke}],
;;      :form (do (js/alert (pr-str 'greetings x))),
;;      :op :do}],
;;    :name {:name hello},
;;    :form (fn* ([x] (js/alert (pr-str 'greetings x)))),
;;    :op :fn}],
;;  :name cljs.user/hello,
;;  :form
;;  (def hello (cljs.core/fn ([x] (js/alert (pr-str 'greetings x))))),
;;  :op :def}

(comp/emit ast)
;; cljs.user.hello = (function cljs$user$hello(x) {
;;     return alert(cljs.user.pr_str.call(null,new cljs.core.Symbol(null,"greetings","greetings",-547008995,null),x));
;; });

(defn read-file
  "Read the contents of filename as a seq of clojure values"
  [filename]
  (let [eof (Object.)]
    (with-open [reader (LineNumberingPushbackReader. (io/reader filename))]
      (doall
       (take-while #(not= % eof)
                   (repeatedly #(read reader false eof)))))))

(defn file-ast
  "Return the clojurescript AST for the contents of filename. Tends to be large
  and contain cycles -- be careful printing at the repl."
  [filename]
  (binding [ana/*cljs-ns* 'cljs.user
            ana/*cljs-file* filename]
    (mapv #(ana/analyze (ana/empty-env) %)
          (read-file filename))))

(defn flatten-ast
  [ast]
  (mapcat #(tree-seq :children :children %) ast))

(def flat-ast (flatten-ast (file-ast "src/joy/music.cljs")))

(defn get-interop-used
  "Return a set of symbols representing the method and field names used indentinterop forms"
  [flat-ast]
  (set (keep #(some % [:method :field]) flat-ast)))

(defn externs-for-interop
  [syms]
  (apply str
         "var DummyClass={};\n"
         (map #(str "DummyClass." % "=function(){};\n")
              syms)))
;; => 

;; (spit "externs.js" (externs-for-interop (get-interop-used flat-ast)))
