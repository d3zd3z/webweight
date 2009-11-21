(ns org.davidb.webweight.http
  (:use [compojure])
  (:use [clojure.contrib def str-utils])
  (:use [org.davidb.contrib map-utils])
  (:require [org.davidb.contrib [xml :as xml] [html :as html]])
  (:require [org.davidb.webweight
             [index :as index]
             [daily :as daily]
             [weekly-report :as weekly-report]]))

(defn gen-weight
  [params]
  (if-let [date (:date params)]
    (or (if-let [weekly-file (index/lookup-date date)]
          (-> weekly-file
            (daily/decode-file)
            (weekly-report/generate)
            ((partial xml/->string xml/xhtml1-strict))))
        (page-not-found))
    (xml/->string xml/xhtml1-strict (index/generate))))

(defn htmlify-request
  [request]
  (let-map [[k v] request]
    (html/dl
      (html/dt (html/b (name k)))
      (html/dd (pr-str v)))))

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
  (ANY "*"
       (xml/->string
         xml/xhtml1-strict
         (html/html
           :attrs xml/xhtml1-attrs
           (html/head (html/title "404 - Not found"))
           (apply html/body
                  (html/h1 "404 - Not found")
                  (htmlify-request request)))))
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
