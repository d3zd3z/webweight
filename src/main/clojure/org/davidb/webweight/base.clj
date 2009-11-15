(ns org.davidb.webweight.base
  (:use [clojure.contrib def])
  (:import [java.io File]))

(defonce #^String root "/home/davidb/weight")
(defonce #^File root-file (File. root))
