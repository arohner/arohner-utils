(ns arohner.map)

(defn submap?
  "True if every key and value in A is present and = in B"
  [a b]
  (every? (fn [[k v]]
            (and (contains? b k)
                 (= (get b k) v))) (seq a)))