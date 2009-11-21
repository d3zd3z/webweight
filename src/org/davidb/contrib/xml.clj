(ns org.davidb.contrib.xml
  (:use clojure.xml)
  (:use [clojure.contrib [pprint :only [pprint]]])
  (:import [clojure.lang Named]))

;;; Fixes for some deficiencies to Clojure's XML handling.  Parse seems to
;;; work OK, but generation is lacking.
;;;
;;; However, the parse produces unescaped XML.

;;; We disallow raw XML in the string content.

;;; Some utilities to make it easier to construct XML elements and the
;;; tree.  Also can check that the tree is valid as it is being built.

(defn- check-content
  "Validate the content."
  [content]
  (doseq [item content]
    (when-not
      (or (string? item)
          (keyword? (:tag item)))
      (throw (Exception. (str "Invalid content item: " item)))))
  content)

(defonce adding-attrs ::flag-these-arguments-as-attributes)

(defn- decode-xml-args
  "Remove XML attribute arguments from the beginning.  Removes pairs
  of arguments that are either a [keyword/symbol string] to add a
  single attribute named by the keyword or symbolc, a pair of the
  magic 'xml/adding-attrs' followed by a map from keywords to values
  for additional arguments.  Returns an attribute map followed by the
  rest of the arguments followed by the first argument not matching
  this pattern."
  [args]
  (loop [[k v & more :as args] args
         attrs []]
    (cond
      (and (instance? Named k)
           (string? v))
      (recur more (conj attrs [k v]))

      (= k adding-attrs)
      (recur more (into attrs v))

      :else [(into {} attrs) args])))

(defn elt
  "Build an xml element.  Tag must be a keyword.  This is followed by
  zero or more pairs of arguments of either [Keyword/Symbol String]
  for individual attributes, or [xml/adding-attrs Map] to add a mapping
  to the attribute list.  The rest of the arguments are the contents."
  [tag & args]
  (let [[atts content] (decode-xml-args args)]
    (struct element tag atts (check-content content))))


(defn eltcat
  "Like 'elt', but the rest of the arguments should each be sequences.
  All of the sequences will be concatted together to make the
  contents."
  [tag & args]
  (let [[atts content] (decode-xml-args args)]
    (struct element tag atts (check-content (apply concat content)))))

;;; Then, a new emit, since the Clojure one is completely wrong, and
;;; doesn't properly escape characters.

(defn escape-xml
  "Change invalid XML characters into entities."
  [string]
  (.. #^String string
    (replace "&" "&amp;")
    (replace "<" "&lt;")
    (replace ">" "&gt;")
    (replace "\"" "&quot;")))

;;; Non-functional, StringBuilder version.
(def *out-writer*)
(defn- append
  "Append one or more strings.  Must be String."
  [& text]
  (doseq [piece text]
    (.append #^StringBuilder *out-writer* #^String piece)))

(defn- add-attributes
  "Add the map of attributes.  The keys must be either strings or keywords,
  and are not checked for valid characters.  The values must be Strings and
  will be escaped."
  [atts]
  (doseq [[k v] atts]
    (let [k (if (instance? Named k) (name k) k)]
      (append " " k "=\"" (escape-xml v) "\""))))

(defn- add-element
  [e]
  (if (instance? String e)
    (append (escape-xml e))
    (do
      (append "<" (name (:tag e)))
      (add-attributes (:attrs e))
      (if-let [content (seq (:content e))]
        (do
          (append ">")
          (doseq [c content]
            (add-element c))
          (append "</" (name (:tag e)) ">"))
        (append " />")))))

(defn ->string
  ([doctype tree]
   (let [s (StringBuilder.)]
     (binding [*out-writer* s]
       (append "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
       (when (string? doctype)
         (append doctype "\n"))
       (add-element tree))
     (str s)))
  ([tree]
   (->string nil tree)))

#_ (defn ->string
  [doctype tree]
  (with-out-str
    (pprint tree)))

(def xhtml1-transitional
  "<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(def xhtml1-strict
  "<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")

(def xhtml1-attrs
  {:lang "en"
   "xml:lang" "en"
   :xmlns "http://www.w3.org/1999/xhtml"})
