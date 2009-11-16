(ns org.davidb.contrib.xml
  (:use clojure.xml)
  (:use [clojure.contrib [pprint :only [pprint]]])
  (:import [clojure.lang Named])
  (:import [org.apache.commons.lang StringEscapeUtils]))

;;; Fixes for some deficiencies to Clojure's XML handling.  Parse seems to
;;; work OK, but generation is lacking.
;;;
;;; However, the parse produces unescaped XML.

;;; We disallow raw XML in the string content.

;;; TODO: detect legal xhtml entities and convert the to named
;;; entities.
;;; TODO: Convert out of range characters to &#...; notation.

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

(defn node
  "Build an xml element.  The tag must be a keyword.  The optional attributes
  must be a map of keywords or strings to strings.  The contents must
  be a sequence of either elements, or strings."
  ([tag]
   (struct element tag))
  ([tag content]
   (struct element tag nil (check-content content)))
  ([tag atts content]
   (struct element tag atts (check-content content))))

(defn elt
  "Build an xml element.  Tag must be a keyword.  This can be followed by
  :attrs map to give attributes.  The rest must be the contents of the
  element."
  [tag & args]
  (if (= (first args) :attrs)
    (struct element tag (second args)
            (check-content (drop 2 args)))
    (struct element tag nil (check-content args))))

(defn eltcat
  "Like 'elt', but the arguments should each be sequences.  All of the
  sequences will be concatted together to make the contents."
  [tag & args]
  (if (= (first args) :attrs)
    (struct element tag (second args)
            (check-content (apply concat (drop 2 args))))
    (struct element tag nil (check-content (apply concat args)))))

;;; Then, a new emit, since the Clojure one is completely wrong, and
;;; doesn't properly escape characters.

(defn escape-xml
  "Change invalid XML characters into entities."
  [string]
  (StringEscapeUtils/escapeHtml #^String string))

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
