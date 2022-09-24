(defproject ch13-clojurescript "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojurescript "1.10.339"]]
  :plugins [[lein-cljsbuild "1.1.8"]]
  :main ^:skip-aot ch13-clojurescript.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :cljsbuild
  {:builds
   [;;{:source-paths ["src"]
     ;;:compiler
     ;;{:output-to "dev-target/all.js"
     ;; :optimizations :whitespace
      ;;:pretty-print true}}
    {:source-paths ["src"]
     :compiler
     {:output-to "prod-target/all.js"
      :optimizations :advanced
      :externs ["externs.js"]
      :pretty-print false}}]})
