(ns org.davidb.webweight.base
  (:use [clojure.contrib def])
  (:import [java.io File]))

(defonce #^String root
  (str
    (System/getProperty "user.home")
    (System/getProperty "file.separator")
    "weight"))
(defonce #^File root-file (File. root))
