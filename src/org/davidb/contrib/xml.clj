(ns org.davidb.contrib.xml
  (:use clojure.xml)
  (:use [clojure.contrib [pprint :only [pprint]]])
  (:import [clojure.lang Named])
  (:import [java.io Writer StringWriter])
  (:import [org.xml.sax.helpers AttributesImpl])
  (:import [javax.xml.transform OutputKeys])
  (:import [javax.xml.transform.sax SAXTransformerFactory TransformerHandler])
  (:import [javax.xml.transform.stream StreamResult])
  )

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

;;; Use a transformer from Java's XML library to render the XML.

(def xhtml-ns "http://www.w3.org/1999/xhtml")
(def xhtml1-transitional
  {:public "-//W3C//DTD XHTML 1.0 Transitional//EN"
   :system "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
   :ns xhtml-ns})
(def xhtml1-strict
  {:public "-//W3C//DTD XHTML 1.0 Strict//EN"
   :system "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
   :ns xhtml-ns})

;(def sample (elt :html
;                 :stuff "val\"ue of stuff"
;                 :another "\u2022Th<is\u00a0is v&er>y s\"imple"
;                 :and "this is another fairly long attribute"
;                 :more "to see how it does at formatting them"
;                 (elt :p :class "stuff")))

(defn- #^TransformerHandler setup-transformer
  "Construct a document transformer, ready to have data pushed to it,
  transforming to the given writer.  Returns the TransformerHandler."
  [doctype, #^Writer writer]
  (let [result (StreamResult. writer)
        tf #^SAXTransformerFactory (SAXTransformerFactory/newInstance)
        hd (.newTransformerHandler tf)
        trans (.getTransformer hd)]
    (.setOutputProperty trans OutputKeys/ENCODING "UTF-8")
    (when doctype
      (.setOutputProperty trans OutputKeys/DOCTYPE_PUBLIC (:public doctype))
      (.setOutputProperty trans OutputKeys/DOCTYPE_SYSTEM (:system doctype)))
    (.setOutputProperty trans OutputKeys/METHOD "xml")
    (.setOutputProperty trans OutputKeys/INDENT "yes")
    (.setResult hd result)
    hd))

(defn write-xml
  "Write the xml document 'tree' to the given 'writer', using
  information from the doctype specifier."
  [doctype writer tree]
  (let [hd (setup-transformer doctype writer)
        atts (AttributesImpl.)
        walk (fn walk [node]
               (if (string? node)
                 (.characters hd (.toCharArray #^String node) 0 (count node))
                 (do
                   (.clear atts)
                   (doseq [[k v] (:attrs node)]
                     (.addAttribute atts "" "" (name k) "CDATA" v))
                   (.startElement hd "" "" (name (:tag node)) atts)
                   (doseq [child (:content node)]
                     (walk child))
                   (.endElement hd "" "" (name (:tag node))))
               ))]
    (.startDocument hd)
    (walk tree)
    (.endDocument hd)))

(defn ->string
  [doctype tree]
  (let [writer (StringWriter.)]
    (write-xml doctype writer tree)
    (str writer)))

(def xhtml1-attrs
  {:lang "en"
   ':xml:lang "en"
   :xmlns "http://www.w3.org/1999/xhtml"})
