(ns org.davidb.webweight.daily
  (:use [clojure.contrib def cond fcase
         [duck-streams :only [read-lines]]])
  (:require [org.danlarkin [json :as json]])
  (:require [org.davidb.webweight [base :as base]])
  (:import [java.io File FileReader])
  (:import [java.util Calendar Date])
  (:import [java.text DateFormat ParsePosition SimpleDateFormat]))

;;; The grammar of this file allows an 'assignment' of an active
;;; weight that gets used for subsequent daily calculations.
;;; Eventually, this is going away, and the real interpolated weight
;;; data will be used instead.  For now, keep this around to hold a
;;; value for it.

(defvar- #^DateFormat date-parser (SimpleDateFormat. "yyyy-MM-dd"))
(defvar- #^DateFormat time-parser (SimpleDateFormat. "HH:mm"))

(defvar- *foods* nil)
(defn- read-foods
  "Read the foods database, and return it."
  []
  (with-open [f (FileReader. (File. base/root "foods.json"))]
    (json/decode-from-reader f)))

(defn- qualify-food
  "Given a tagged item, look up the tag and the kind in the food database
  and augment the item with a :calories and :total-calories field."
  [item]
  (if-let [cals (get-in *foods* [(:tag item) (:kind item)])]
    (assoc item :calories cals
           :total-calories (* cals (:count item)))
    (throw (Exception. (str "Unknown item: " item)))))

;;; Each line of the file matches one of these regexps.
(defn- decode
  [line]
  (cond-let [m]
    (re-matches #"^weight\s*=\s*([\d\.]+)$" line)
    {:tag :weight, :value (Double/parseDouble (nth m 1))}

    (re-matches #"^\s*$" line)
    {:tag :blank}

    (re-matches #"^(\d\d\d\d-\d\d-\d\d):$" line)
    {:tag :date, :value (.parse date-parser (nth m 1) (ParsePosition. 0))}

    (re-matches #"^\s+(\d\d:\d\d) ([SEB]) ([\w+-]+)$" line)
    (qualify-food
      {:tag (keyword (nth m 2)), :time (nth m 1), :count 1,
       :kind (keyword (nth m 3))})

    (re-matches #"^\s+(\d\d:\d\d) (PA) (\d+) (.*)$" line)
    {:tag :PA, :time (nth m 1), :count (Double/parseDouble (nth m 3)),
     :description (nth m 4)}

    (re-matches #"^\s+(\d\d:\d\d) ([VFP]) (\d+) ([\w+-]+)$" line)
    (qualify-food
      {:tag (keyword (nth m 2)), :time (nth m 1),
       :count (Double/parseDouble (nth m 3)),
       :kind (keyword (nth m 4))})

    (throw (Exception. (str "Unknown line: " line)))))

(defn- parse-records
  "Parse the tagged lines returned by 'decode', returning a sequence
  of the days represented."
  [coll]
  (loop [date nil
         weight -1
         [record & rest-records :as all-records] coll
         [cur-node & rest-nodes :as all-nodes] ()]
    (if record
      (case (:tag record)
            :blank (recur date weight rest-records all-nodes)
            :weight (recur date (:value record) rest-records all-nodes)
            :date (recur (:value record) weight rest-records (cons [] all-nodes))
            (if (zero? (count all-nodes))
              (throw (Exception. (str "Cannot start record entry without preceeding date: " record)))
              (recur date weight rest-records
                     (cons (conj cur-node
                                 (assoc record
                                        :weight weight
                                        :date date))
                           rest-nodes))))
      (reverse all-nodes))))

(defn decode-file
  "Decode the contents of the file into a lazy sequence of tagged matches."
  [path]
  (binding [*foods* (read-foods)]
    (parse-records (map decode (read-lines path)))))
