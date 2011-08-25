(ns arohner.set
  (:require [clojure.set :as set]))

(defn disjoint? [a b]
  (->> (set/intersection a b)
       (count)
       (zero?)))