(ns org.davidb.webweight.foods
  (:require [org.davidb.webweight
             [base :as base]])
  (:use [clojure.contrib.json [read :only [read-json]]])
  (:use [clojure.walk :only [keywordize-keys]]))

(defn load-foods
  ([] (load-foods (str base/root "/foods.json")))
  ([path]
   (with-open [rd (java.io.FileReader. #^String path)
               rd (java.io.PushbackReader. rd)]
     (keywordize-keys (read-json rd)))))
