(ns org.davidb.webweight.foods
  (:require [org.davidb.webweight
             [base :as base]])
  (:use [clojure.walk :only [keywordize-keys walk]])
  (:import [org.yaml.snakeyaml Yaml])
  (:import [java.util Map]))

(defn deep-mapify
  "Convert a Java map that isn't a clojure map into a clojure map.
  Deeply converts the fields of the map as well."
  [form]
  (let [form (if (and (not (map? form))
                      (instance? Map form))
               (into {} form)
               form)]
    (walk deep-mapify identity form)))

(defn load-foods
  "Load the food database from the YAML store."
  ([] (load-foods (str base/root "/foods.yaml")))
  ([path]
   (with-open [rd (java.io.FileReader. #^String path)]
     (let [yaml (Yaml.)
           data (.load yaml rd)
           data (deep-mapify data)
           data (keywordize-keys data)]
       data))))
