(ns org.davidb.contrib.xml
  (:use clojure.xml)
  (:use [clojure.contrib [pprint :only [pprint]]])
  (:import [clojure.lang Named])
  (:import [java.io StringWriter])
  (:import [org.w3c.dom Document Node])
  (:import [javax.xml.stream XMLEventFactory XMLOutputFactory])
  (:import [javax.xml.transform OutputKeys TransformerFactory])
  (:import [javax.xml.transform.dom DOMSource])
  (:import [javax.xml.transform.stream StreamResult])
  (:import [javax.xml.parsers DocumentBuilderFactory]))

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

;(def factory (XMLEventFactory/newInstance))
;(def factory (.createXMLStreamWriter (XMLOutputFactory/newInstance) (StringWriter.)))

;; Things to do:
;; Handle non-default namespaces.
(defn old->string
  [doctype tree]
  (let [output-factory (XMLOutputFactory/newInstance)
        writer (StringWriter.)
        out (.createXMLStreamWriter output-factory writer)
        walk (fn walk [node top]
               (cond
                 (string? node)
                 (.writeCharacters out node)

                 (empty? (:content node))
                 (do
                   (.writeEmptyElement out (name (:tag node)))
                   (doseq [[k v] (:attrs node)]
                     (.writeAttribute out (name k) v)))

                 :else
                 (do
                   (.writeStartElement out (name (:tag node)))
                   (doseq [[k v] (:attrs node)]
                     (.writeAttribute out (name k) v))
                   (doseq [child (:content node)]
                     (walk child false))
                   (.writeEndElement out))
               ))]
    (.writeStartDocument out "UTF-8" "1.0")
    (.writeDTD out doctype)
    (.writeEndDocument out)
    ;; Setting default namespace.
    (walk tree true)
    (.flush out)
    (str writer)))

(def xhtml1-transitional
  "<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(def xhtml1-strict
  "<!DOCTYPE html
  PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
(def xhtml-ns "http://www.w3.org/1999/xhtml")

(def sample (elt :html
                 :stuff "val\"ue of stuff"
                 :another "\u2022Th<is\u00a0is v&er>y s\"imple"
                 :and "this is another fairly long attribute"
                 :more "to see how it does at formatting them"
                 (elt :p :class "stuff")))

(defn #^Document make-document
  "Construct an empty DOM document."
  []
  (let [factory (DocumentBuilderFactory/newInstance)
        builder (.newDocumentBuilder factory)
        impl (.getDOMImplementation builder)
        doc-type (.createDocumentType impl "html"
                                      "-//W3C//DTD XHTML 1.0 Transitional//EN"
                                      "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")]
    (.createDocument impl "http://www.w3.org/1999/xhtml" "html" doc-type)))

;;; Build a DOM Document out of a Clojure XML representation.
(defn xml->DOM
  [tree]
  (let [top (make-document)
        walk (fn walk [#^Node parent node]
               (if (string? node)
                 (.appendChild parent (.createTextNode top node))
                 (let [child (.createElement top (name (:tag node)))]
                   (doseq [[k v] (:attrs node)]
                     (.setAttribute child (name k) v))
                   (doseq [x (:content node)]
                     (walk child x))
                   (.appendChild parent child))))
        root (.getDocumentElement top)]
    ;(walk top tree)
    (doseq [[k v] (:attrs tree)]
      (.setAttribute root (name k) v))
    (doseq [x (:content tree)]
      (walk root x))
    (.setXmlVersion top "1.0")
    ;(.setDocumentURI top "http://www.w3.org/1999/xhtml")
    top))

(defn DOM->string
  [dom]
  (let [trans (.newTransformer (TransformerFactory/newInstance))
        writer (StringWriter.)]
    (.setOutputProperty trans OutputKeys/OMIT_XML_DECLARATION "no")
    (.setOutputProperty trans OutputKeys/METHOD "xml")
    (.setOutputProperty trans OutputKeys/INDENT "yes")
    (.setOutputProperty trans OutputKeys/STANDALONE "no")
    (.setOutputProperty trans OutputKeys/DOCTYPE_PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN")
    (.setOutputProperty trans OutputKeys/DOCTYPE_SYSTEM "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    (.transform trans (DOMSource. dom) (StreamResult. writer))
    (str writer)))

(defn ->string
  ;;; Wrong calling convention here.
  [doctype tree]
  (DOM->string (xml->DOM tree)))

(def xhtml1-attrs
  {:lang "en"
   ':xml:lang "en"
   :xmlns "http://www.w3.org/1999/xhtml"})
