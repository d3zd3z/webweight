(ns org.davidb.webweight.foods
  (:require [org.davidb.webweight
             [base :as base]])
  (:require [org.danlarkin.json :as json]))

(defn load-foods
  ([] (load-foods (str base/root "/foods.json")))
  ([path]
   (with-open [rd (java.io.FileReader. #^String path)]
     (json/decode-from-reader rd))))
