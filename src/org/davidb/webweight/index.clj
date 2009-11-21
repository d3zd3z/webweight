(ns org.davidb.webweight.index
  (:use [compojure :only [xhtml-tag]])
  (:use [clojure.contrib def
         [seq-utils :only [partition-by]]])
  (:require [org.davidb.contrib [xml :as xml] [html :as html]])
  (:require [org.davidb.webweight
             [base :as base]])
  (:import [java.io File])
  (:import [java.util Calendar Date])
  (:import [java.text DateFormat ParsePosition SimpleDateFormat]))

(defvar- #^File old-base (File. base/root-file "old"))

;;; The weight directory contains a series of files in the form of
;;; YYYY-MM-DD.dat either at the root, or in a subdirectory called
;;; 'old'.  The files should be named after the first date mentioned
;;; in the file.
(def #^DateFormat date-parser (SimpleDateFormat. "yyyy-MM-dd'.dat'"))
(def #^DateFormat request-parser (SimpleDateFormat. "yyyy-MM-dd"))
(def #^DateFormat human-date-parser (SimpleDateFormat. "EE MMM d"))
(def #^DateFormat url-date-parser (SimpleDateFormat. "'?date='yyyy-MM-dd"))
(def #^DateFormat month-parser (SimpleDateFormat. "yyyy-MMMM"))
(def #^DateFormat year-parser (SimpleDateFormat. "yyyy"))

(defn path->date
  "Convert a filename pattern into a date.  Returns nil if the format is
  not correct."
  [#^String path]
  (let [pos (ParsePosition. (inc (.lastIndexOf path "/")))]
    (.parse date-parser path pos)))

(defn date->path
  "Convert a date into a filename appropriate for it."
  [#^Date date]
  (.format date-parser date))

(defn resolve-one-date
  "Returns either a File for this date in the given path, or nil, if it is
  not found."
  [#^Date date, #^File path]
  (let [path (File. path #^String (date->path date))]
    (if (.exists path) path)))

(defn resolve-date
  "Given a date, return the File associated with that date."
  [date]
  (or (resolve-one-date date base/root-file)
      (resolve-one-date date old-base)))

(defn lookup-date
  "Given a date from a query, return either a File for it, or nil"
  [text]
  (when-let [date (.parse request-parser text (ParsePosition. 0))]
    (resolve-date date)))

(defn get-logs
  "Retrieve all available logs, returning a sorted sequence of dates,
  with the newest at the top."
  []
  (-> (concat (.list base/root-file) (.list old-base))
    ((partial map path->date))
    ((partial filter identity))
    (sort)
    (reverse)))

(defn get-date-field
  [#^Date date, field]
  (-> (doto (Calendar/getInstance)
        (.setTime date))
    (.get field)))

(defn by-years-and-months
  "Group the dates by year and then months within the years."
  [dates]
  (let [dates (partition-by #(get-date-field % Calendar/YEAR) dates)]
    (map (fn [d] (partition-by #(get-date-field % Calendar/MONTH) d))
         dates)))

(defn encode-date
  "HTML tree encoding a date and it's link.  Result is flat."
  [#^Date date]
  (html/a :href (.format url-date-parser date)
          (.format human-date-parser date)))

(defn encode-month
  "Given a sequence of dates that fall in a given month, generate
  a sequence of html nodes indicating these."
  [coll]
  [(html/h3 (.format month-parser (first coll)))
   (apply html/p
          (interpose "\u00a0 \u00a0"
                     (map encode-date coll)))])

(defn encode-year
  "Given a sequence of sequences of dates falling in a given year,
  generate an HTML sequence tree rendering this."
  [coll]
  (cons
    (html/h2 (.format year-parser (first (first coll))))
    (apply concat (map encode-month coll))))

(defn generate
  "Generate the index based on the current log files."
  []
  (html/html
    xml/adding-attrs xml/xhtml1-attrs
    (html/head (html/title "Weight reports"))
    (apply html/body
           (html/h1 "Weight reports")
           (apply concat (map encode-year (by-years-and-months (get-logs)))))))
