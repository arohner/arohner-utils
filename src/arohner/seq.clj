(ns arohner.seq)

(defn map-keys 
  "returns a new map with f applied to the keys of the map. f should
  supply a unique value for each key in map, or duplicate keys will be
  overwritten."
  [f m]
  (into {} (map (fn [[key val]]
                  [(f key) val]) m)))

(defn map-vals 
  "returns a new map with f applied to the vals of the map"
  [f m]
  (into {} (map (fn [[key val]]
                  [key (f val)]) m)))

(defn ffilter
  "(first (filter ...))"
  [f seq]
  (first (filter f seq)))

(defn partition-at
  "splits coll each time f is truthy

  (partition-transition div3? (range 10)) => ((0 1 2) (3 4 5) (6 7 8) (9))"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f fst)
           run (cons fst (take-while #(not= fv (f %)) (rest s)))]
       (cons run (partition-at f (drop (count run) s)))))))