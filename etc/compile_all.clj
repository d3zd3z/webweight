(ns compile-all
  (:require [clojure.contrib.find-namespaces :as fn])
  (:import [java.io File]))

;;; This utility expects 2 command line arguments giving the source
;;; directory and the destination directory, both of which must
;;; already be in the classpath.

;;; If we were invoked without a fork from Ant, then we will not be
;;; able to exit, so raise a general exception.
(defn fail [msg]
  (println msg)
  (try (System/exit 1)
    (catch Exception e
      (throw (Exception. "Compilation failed")))))

(when-not (= (count *command-line-args*) 2)
  (fail "Expecting two args: srcdir destdir"))

(def #^File srcdir (File. (first *command-line-args*)))
(def #^File destdir (File. (second *command-line-args*)))

(def compile-ns (fn/find-namespaces-in-dir srcdir))

;;; Only recompile the files that are newer.  The Clojure compiler
;;; already uses the same logic, but will still load the resulting
;;; class.  By filtering beforehand, we can eliminate the loads on
;;; classes.
;;;
;;; Also note that this logic doesn't detect use of macros causing the
;;; need to recompile.  Doing that would require analyzing a full
;;; dependency tree, which might be doable, since find-namespaces
;;; already had to parse the namespace declaration.
(defn file-for-ns
  [ns, #^File dir, suffix]
  (let [fixed-name (.. (name ns)
                     (replace "." "/")
                     (replace "-" "_"))
        path (File. dir (str fixed-name suffix))]
    path))
(defn needs-compile?
  [ns]
  (let [src (file-for-ns ns srcdir ".clj")
        dest (file-for-ns ns destdir "__init.class")]
    (when (not (.exists src))
      (printf "Inconsistent path/ns issue with namespace %s\n" (name ns))
      (fail "Make sure file name matches included namespace"))
    (or (not (.exists dest))
        (> (.lastModified src) (.lastModified dest)))))

(def to-compile (filter needs-compile? compile-ns))
(def compile-count (count to-compile))

;;; TODO: Make a first guess as to which classes actually need to be
;;; compiled.  The compiler will just load classes that don't need to
;;; be compiled, but it would be nice to only print messages out.

(when (pos? compile-count)
  (printf "Clojure: compiling %d namespaces to %s\n" compile-count
          (.getPath destdir))
  (flush))

(defn show-causes [e]
  (loop [e e]
    (when e
      (println (str e))
      (recur (.getCause e)))))

(binding [*compile-path* (second *command-line-args*)]
  (doseq [n to-compile]
    ;(printf "Compiling %s\n" (name n))
    ;(flush)
    ;;; The stack trace is not really meaningful in the compiler.
    ;;; But, the current compiler seems to cause some strange
    ;;; exceptions.  For now, just print the cause trace without the
    ;;; stacks, and then exit.
    (try
      (compile n)
      (catch Throwable e
        (show-causes e)
        (fail "Compilation failure")))))
