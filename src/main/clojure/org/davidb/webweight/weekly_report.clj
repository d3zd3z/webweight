(ns org.davidb.webweight.weekly-report
  (:use [org.davidb.contrib.map-utils])
  (:use [clojure.set])
  (:use [clojure.contrib def])
  (:require [compojure :as compojure])
  (:import [java.text DateFormat SimpleDateFormat]))

(defvar- #^DateFormat short-date-gen (SimpleDateFormat. "EEE MMM dd"))
(defvar- #^DateFormat date-gen (SimpleDateFormat. "EEE MMM dd, yyyy"))
(defvar- #^DateFormat dow-gen (SimpleDateFormat. "EEE"))
(defvar- #^DateFormat iso-date-gen (SimpleDateFormat. "yyyy-MM-dd"))

;;; The output report is similar to a typical spread sheet.  The
;;; groups of data for each day are summarized onto each line.  Some
;;; fields are accumulated, and others are computed at the end.

(defvar daily-sum-fields
  [:S :C :E :B :F :V :P :PA :total-calories]
  "Fields from the records that are folded to produce an summary.")
(defvar empty-day
  (into {} (map (fn [k] [k 0]) daily-sum-fields)))
(defn morph-record
  "Add a field to a record named after it's tag with the count as
  it's value."
  [record]
  (if-let [tag (:tag record)]
    (assoc record tag (:count record))
    record))
(defvar daily-sum
  (into {}
        (let-map [k daily-sum-fields]
          [k +]))
  "Map associating a combining function for summarizing values.")

(defn record-combine
  "Combines two records.  The sum-map gives a function that will be
  used to combine fields from the two records.  For each key that is
  present in all three maps, the function from 'sum-map' will be
  applied to the values in 'a' and 'b' to produce the new value for
  this field.  Otherwise, the field will be combined as per 'merge'."
  [sum-map a b]
  (let [a-keys (set (keys a))
        b-keys (set (keys b))
        sum-keys (set (keys sum-map))
        inter-keys (intersection a-keys b-keys sum-keys)
        updates (map (fn [k] [k ((get sum-map k) (get a k) (get b k))]) inter-keys)
        updates (into {} updates)]
    (merge a b updates)))

(defn daily-combine
  "Combine two records  within a single day."
  [a b]
  (record-combine daily-sum a b))

(defn daily-computation
  "Perform some interfield computations for a single day."
  [daily]
  (let [total-cals (:total-calories daily)
        pa-cals (:PA daily)
        net-cals (- total-cals pa-cals)
        weight (:weight daily)
        daily-change (/ (- net-cals (* 13 weight)) 500.0)]
    (assoc daily
           :days 1
           :net-cals net-cals
           :daily-change daily-change)))

(defn fold-day
  "Fold all of the records from one day together."
  [coll]
  (daily-computation
    (reduce daily-combine empty-day
            (map morph-record coll))))

(defstruct column :title :field :formatter)
(defn st
  "Generate a formatter for the given format spec."
  [fmt]
  #(format fmt (double %)))

(defvar columns
  [(struct column "Day" :date #(.format dow-gen %))
   (struct column "Date" :date #(.format iso-date-gen %))
   (struct column "S" :S (st "%.0f"))
   (struct column "C" :C (st "%.0f"))
   (struct column "E" :E (st "%.0f"))
   (struct column "F" :F (st "%.0f"))
   (struct column "V" :V (st "%.0f"))
   (struct column "PA" :PA (st "%.0f"))
   (struct column "Total Cals" :total-calories (st "%.0f"))
   (struct column "Net Cals" :net-cals (st "%.0f"))
   (struct column "Daily Weight" :daily-change (st "%.2f"))
   (struct column "Net Weight" :net-change (fn [_] "todo"))])
(defvar table-header
  `[:tr
    ~@(map (fn [r] [:th (:title r)]) columns)])
(defvar table-sep
  `[:tr
    ~@(replicate (count columns) [:th])])
(defn make-table-row
  [daily]
  (let [fields
        (let-map [col columns]
          (let [item (get daily (:field col))]
            [:td ((:formatter col) item)]))]
    `[:tr ~@fields]))

(defn make-table
  [weekly]
  `[:table {:border 1}
    ~table-header
    ~@(map make-table-row (map fold-day weekly))
    ~table-header])

(defn get-date-range
  "Extract the date range out of the user-supplied data."
  [weekly]
  (let [start (:date (first (first weekly)))
        end (:date (first (last weekly)))]
    (format "%s through %s"
            (.format short-date-gen start)
            (.format date-gen end))))

(defn generate
  "Generate a weekly report from the weekly record."
  [weekly]
  `[:html
    [:head
     [:link {:rel "stylesheet",
             :type "text/css",
             :href "/style/clean-table.css"}]
     [:title "Weight report"]]
    [:body
     [:h1 ~(get-date-range weekly)]
     [:p [:a {:href "?"} "&lt;= Back to index"]]
     ~(make-table weekly)]])

(use '[org.davidb.webweight.daily :as daily])
(def x (daily/decode-file (java.io.File. "/home/davidb/weight/2009-11-11.dat")))
;(def a (fold-day (first x)))
