(ns ch12-java.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.sun.net.httpserver HttpHandler HttpExchange HttpServer]
           [java.net InetSocketAddress URLDecoder URI]
           [java.io File FilterOutputStream])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
   (respond identity exchange body))
  ([around exchange body]
   (.sendResponseHeaders exchange OK 0)
   (with-open [resp (around (.getResponseBody exchange))]
     (.write resp (.getBytes body)))))

(defn new-server
  [port path handler]
  (doto
      (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

(defn default-handler
  [txt]
  (proxy [HttpHandler]  ;; extend
      []                ;; no actor args
    (handle [exchange]  ;; override
      (respond exchange txt))))

;; (def server
;;   (new-server
;;    8123
;;    "/joy/hello"
;;    (default-handler "Hello Jakarta")))

(def p (default-handler
        "There's no problem that cant be solved with another level of indirection"))

(def server (new-server 8123 "/" p))

(update-proxy p
              {"handle" (fn [this exchange]
                          (respond exchange (str "this is " this)))})

(def echo-handler
  (fn [_ exchange]
    (let [headers (.getRequestHeaders exchange)]
      (respond exchange (prn-str headers)))))

(update-proxy p {"handle" echo-handler})

(defn html-around
  [o]
  (proxy [FilterOutputStream]
      [o]
    (write [raw-bytes]
      (proxy-super write
                   (.getBytes (str "<html><body>"
                                   (String. raw-bytes)
                                   "</body></html>"))))))

(defn listing
  [file]
  (-> file .list sort))

(listing (io/file "."))
;; => (".gitignore" ".hgignore" ".nrepl-port" "CHANGELOG.md" "LICENSE" "README.md" "doc" "project.clj" "resources" "src" "target" "test")

(listing (io/file "./README.md"))
;; => ()

(defn html-links
  [root filenames]
  (string/join
   (for [file filenames]
     (str "<a href='"
          (str root
               (if (= "/" root)
                 ""
                 File/separator)
               file)
          "'>"
          file "</a><br>"))))

(html-links "." (listing (io/file ".")))
;; => "<a href='./.gitignore'>.gitignore</a><br><a href='./.hgignore'>.hgignore</a><br><a href='./.nrepl-port'>.nrepl-port</a><br><a href='./CHANGELOG.md'>CHANGELOG.md</a><br><a href='./LICENSE'>LICENSE</a><br><a href='./README.md'>README.md</a><br><a href='./doc'>doc</a><br><a href='./project.clj'>project.clj</a><br><a href='./resources'>resources</a><br><a href='./src'>src</a><br><a href='./target'>target</a><br><a href='./test'>test</a><br>"

(defn details
  [file]
  (str (.getName file) " is "
       (.length file) " bytes."))
;; => #'ch12-java.core/details

(details (io/file "./README.md"))
;; => "README.md is 21 bytes."

(defn uri->file
  [root uri]
  (->> uri
       str
       URLDecoder/decode
       (str root)
       io/file))
;; => #'ch12-java.core/uri->file

(uri->file "." (URI. "/project.clj"))
;; => #object[java.io.File 0x7b196452 "./project.clj"]

(details (uri->file "." (URI. "/project.clj")))
;; => "project.clj is 473 bytes."

(def fs-handler
  (fn [_ exchange]
    (let [uri (.getRequestURI exchange)
          file (uri->file "." uri)]
      (if (.isDirectory file)
        (do (.add (.getResponseHeaders exchange)
                  "Content-Type" "text/html")
            (respond html-around
                     exchange
                     (html-links (str uri) (listing file))))
        (respond exchange (details file))))))

(update-proxy p {"handle" fs-handler})
