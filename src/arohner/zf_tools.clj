(ns arohner.zf-tools
  (:require [clojure.contrib.zip-filter :as zf]
            [clojure.contrib.zip-filter.xml :as zf-xml]
            [clojure.zip :as zip]
            [clojure.xml :as xml])
  (:use [winston.utils :only (inspect if->)]))

;; extra predicates for c.c.zip-filter

(defn child-text
  "Acts like like zf.xml/text, but
  only checks the immediate children, rather than all descendants."
  [loc]
  (.replaceAll
      ^String (apply str (zf-xml/xml-> loc zf/children zip/node string?))
      (str "[\\s" (char 160) "]+") " "))

(defn remove-children [loc]
  (if-let [children (zf/children loc)]
    (do
      (println "removing" (first children))
      (recur (zip/remove (first children))))
    loc))

(defn remove-tag=
  "deletes the tag, and all descendant nodes when tagname matches"
  [tagname]
  (fn [loc]
    (if (some (fn [anc]
                (and (zip/branch? anc)
                     (= tagname (:tag (zip/node anc))))) (zf/ancestors loc))
      nil
      loc)))

(defn remove-attr=
  "deletes the tag with attrname="
  [attrname attrval]
  (fn [loc]
    (if (= attrval (zf-xml/attr loc attrname))
      (zip/remove loc)
      loc)))

(defn child-text-re=
  "returns nodes whose immediate child's text matches the regex "
  [re]
  (fn [loc]
    (boolean (re-find re (child-text loc)))))

(defn child-text-any-re=
  "like child-text-re=, but takes a seq of regexs. returns true if the node matches any of the regexs"
  [res]
  (fn [loc]
    (boolean (some (fn [re]
            (re-find re (child-text loc))) res))))