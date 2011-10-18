(ns arohner.utils
  (:require clojure.pprint))

(defmacro inspect 
  "prints the expression '<name> is <value>', and returns the value"
  [value]
  `(let [result# ~value]
     (println '~value "is" (with-out-str (clojure.pprint/pprint result#)))
     result#))

(defmacro fold 
  "sugar on reduce, similar to how 'for' is a nicer version of 'map'.

  init is a symbol that evaluates to a value (local or var) or a vector of a symbol and an initial value. In the loop, init is bound to the result of the previous loop.

"
;;   (fold r [i (range 10)]   ;; existing symbol
;;       (+ r i))

;; (fold [r 0] [i (range 10)] ;; define new local
;;        (+ r i))
  [initial binding & body]
  (let [init-sym (if (symbol? initial)
                   initial
                   (first initial))
        init-val (if (symbol? initial)
                   initial
                   (second initial))]
    `(let [~init-sym ~init-val]
       (reduce (fn [~init-sym ~(first binding)]
                 ~@body) ~init-sym ~@(rest binding)))))

(defmacro let-> 
   "Provide a name that will be bound to the result of the first form. 
   For each additional form, the variable will be 
   used in the invocation, and then rebound to the result of the form.

   binding can be either an existing symbol (var or local), or a vector of a symbol and initial value (creates a new local)

" 
   [binding & forms]
   
   (let [varname (if (symbol? binding)
                   binding
                   (first binding))
         start (if (symbol? binding)
                   binding
                   (second binding))
         fn-args `[~varname] 
         wrapped (map (fn [form] `(fn ~fn-args ~form)) forms)]
     (reduce 
           (fn [acc func] `(~func ~acc)) 
           start
           wrapped)))

(defn apply-if
  "if test (apply f arg args) else arg"
  [test f arg & args]
  (if test
    (apply f arg args)
    arg))

(defmacro if->
  "if statement meant to be used in arrows. If test is a fn, it is
  called with the argument passed in. Then, true-expr or false-expr is
  called."
  [arg test-fn true-fn false-fn]
  `(if (if (fn? ~test-fn)
         (~test-fn ~arg)
         ~arg)
     (~true-fn ~arg)
     (~false-fn ~arg)))

(defmacro if->>
  [test-fn true-fn false-fn arg]
  `(if (if (fn? ~test-fn)
         (~test-fn ~arg)
         ~arg)
     (~true-fn ~arg)
     (~false-fn ~arg)))

(defn fixed-point
  "iterates until (f (f x)) = (f x)"
  [f x]
  (let [newval (f x)]
    (if (= newval (f newval))
      newval
      (recur f newval))))

(defn inc*
  "increments x, or returns 1 if x is nil"
  [x]
  (if (nil? x)
    1
    (inc x)))

(defn supports-meta?
  "true if metadata can be added to x"
  [x]
  (contains? (ancestors (class x)) clojure.lang.IObj))

(defn update
  "like update-in, but not nested"
  [m k f & args]
  (assoc m k (apply f (get m k) args)))

(defn rename-key
  "in a map, rename the key. overwrites new if it exists"
  [m old new]
  (-> m
      (assoc new (get m old))
      (dissoc old)))

(defn apply-map
  "Takes a fn and any number of arguments. Applies the arguments like
  apply, except that the last argument is converted into keyword
  pairs, for functions that keyword arguments."
  [f & args*]
  (let [normal-args (butlast args*)
        m (last args*)]
    (apply f (concat normal-args (flatten (seq m))))))

