(ns arohner.predicates)

(defn bool?
  "returns true if x is a bool"
  [x]
  (instance? java.lang.Boolean x))