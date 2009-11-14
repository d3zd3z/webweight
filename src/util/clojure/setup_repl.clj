;;; Intended to be invoked from Maven.
;;; Pulls all of the Jar files off of the class path, and copies them
;;; into a 'target/repl-lib' directory so that the repl can be run
;;; directly from a script.

(import '(java.io File))
(require '[clojure.contrib.jar :as jar])
(use 'clojure.contrib.classpath)
(use 'clojure.contrib.shell-out)
(use 'clojure.contrib.java-utils)
;; Rather than the complicated Maven directed assembly target, these
;; utilities help us collect the jarfiles necessary for the program
;; together in one place.  Also, any jarfiles in the build tree will
;; be copied.
(def libdir "target/repl-lib")
(defn clean-lib []
  (letfn [(clean [#^File path]
                 (when (.isDirectory path)
                   (doseq [ent (.listFiles path)]
                     (clean ent)))
                 (.delete path))]
    (clean (File. libdir))))
(defn jar-names []
  (filter jar/jar-file? (classpath)))
(defn copy-jar [#^File jarname]
  (sh "cp" (.getPath jarname)
      (.getPath (file libdir (.getName jarname)))))
(defn install-lib []
  (clean-lib)
  (.mkdir (file libdir))
  (doseq [n (jar-names)]
    (copy-jar n)))
(install-lib)
