(ns org.davidb.webweight.http
  (:use [compojure])
  (:use [clojure.contrib def str-utils])
  (:use [org.davidb.contrib map-utils])
  (:require [org.davidb.webweight
             [index :as index]]))

(defn gen-weight
  [params]
  (if-let [date (:date params)]
    (or (index/lookup-date date)
        (page-not-found))
    (html (index/generate))))

(defn htmlify-request
  [request]
  (->
    (let-map [[k v] request]
      [[:b (escape-html (name k))]
       ": "
       (-> (str v)
         (escape-html)
         ((partial re-gsub #"(\r\n|\r|\n)" "<br />&nbsp;")))])
    ((partial interleave (repeat [[:br]])))
    (rest)
    ((partial apply concat))))

(defroutes
  weight-app
  ;;; Lighttpd < 1.5 can't rewrite with proxies, so catch the various
  ;;; flavors.
  (GET "/:prefix/weight" (gen-weight params))
  (GET "/weight" (gen-weight params))
  ;;; Use wlog until the old data is fully deprecated.
  (GET "/wlog" (gen-weight params))
  (GET "/:prefix/wlog" (gen-weight params))
  ;; (GET "/test" (str params))
  (GET "*"
       (html
         `[:html [:head [:title "404 - Not found"]]
           [:body [:h1 "404 - Not found"] ~@(htmlify-request request)]]))
  #_(ANY "*"
       (page-not-found))
  )

(defonce http-server (agent nil))
(defn start-http []
  (send-off http-server
            (fn [srv]
              (when srv (stop srv) nil)
              (run-server {:port 8080} "/*" (servlet weight-app)))))
(defn stop-http []
  (send-off http-server (fn [srv] (when srv (stop srv)) nil)))
