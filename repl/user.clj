(println "Running user startup")
(use 'clojure.contrib.repl-utils)
(use 'clojure.contrib.str-utils)
(use 'clojure.contrib.java-utils)
(use 'clojure.contrib.def)
(use 'clojure.contrib.ns-utils)
(use 'clojure.contrib.test-is)
(use 'clojure.contrib.sql)
(use 'clojure.contrib.pprint)

(set! *warn-on-reflection* true)

;;; The NS that we are testing/working on.
;;; This can be given through the java property work-ns, e.g.
;;;   ./run.sh -Dwork-ns=org.davidb.chunk-file
(def *work-ns*
  (symbol
    (or (System/getProperty "work-ns")
        'org.davidb.foo)))

;;; A testing NS.  If not given, will use the same name as the
;;; work-ns, with test_ inserted before the last name.
(def *test-ns*
  (symbol
    (or (System/getProperty "test-ns")
        (symbol
          (re-sub #"[^\.]+$" #(str "test_" %)
                  (name *work-ns*))))))

; (use *work-ns*)
(defn l [] (binding [*warn-on-reflection* true]
             (use *work-ns* :reload)))
(defn t []
  (binding [*warn-on-reflection* true]
    (require *test-ns* :reload)
    (run-tests *work-ns* *test-ns*)))
(defn i [] (in-ns *work-ns*))
