(ns arohner.validation)

(defn validate
  "validates an object. validation-seq is a seq of vectors. Each vector contains a fn of one argument that will be applied with obj. If f returns non-truthy, the rest of the arguments will be applied to format. Returns boolean true if validation passed, or the result of format on failure. In format expressions, :$ will be replaced with the input obj, fns will be called with one argument, the input obj

example:
 (validate [[map? \"obj must be a map, got %s\" :$]
            [:type \"m must contain a field :type\"]
            [#(int? (-> % :foo)) \":foo must be an int, got %s\" #(-> % :foo class)] m) "
  [validation-seq obj]
  (loop [vseq validation-seq]
    (if-let [[v-fn & format-args] (first vseq)]
      (if (v-fn obj)
        (recur (rest vseq))
        (apply format (map (fn [arg]
                             (cond
                              (= arg :$) obj
                              (fn? arg) (arg obj)
                              :else arg))  format-args)))
      true)))

(defn validate!
  "same as validate, but returns true or throws an exception containing the validation message on failure."
  [validation-seq obj]
  (let [resp (validate validation-seq obj)]
    (if (string? resp)
      (throw (Exception. resp))
      true)))

(defn valid?
  "same as validate, but always returns a boolean (rather than validation string on failure)"
  [validation-seq obj]
  (not (string? (validate validation-seq obj))))

