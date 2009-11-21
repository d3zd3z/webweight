(ns org.davidb.webweight.main
  (:require [org.davidb.webweight
             [http :as http]])
  (:gen-class))

(defn -main
  [& args]
  (http/start-http))
