(ns org.davidb.contrib.html
  (:use [clojure.contrib def])
  (:require [org.davidb.contrib [xml :as xml]]))

;;; Define functions for all of the commonly used HTML tags.
;;; (tag ...) will expand to (xml/elt tag ...)

(defmacro- deftag
  [tag]
  `(defn ~tag
     ~(str "Make an XML node for the html <"
           tag
           "> tag.\nSame as (apply xml/elt "
           (keyword tag)
           " args)")
     [& args#]
     (apply xml/elt ~(keyword tag) args#)))

(defmacro- deftags
  [& tags]
  (cons 'do
        (map #(list 'org.davidb.contrib.html/deftag %) tags)))

(deftags html head body h1 h2 h3 h4 h5 h6 p hr br table tr th td
         title link col span
         ul li ol dl dt dd a b)
