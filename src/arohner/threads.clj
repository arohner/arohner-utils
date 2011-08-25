(ns arohner.threads
  (:import (java.util.concurrent
            Executors
            ExecutorService)))

(defn bound-future-call 
  "like clojure.core/future-call, but you can specify the thread pool"
  [^Callable f ^ExecutorService executor]
  (let [fut (.submit executor f)]
    (reify 
     clojure.lang.IDeref 
      (deref [_] (.get fut))
     java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defmacro bound-future [& body]
  "like clojure.core/future, but the job is sent to the CPU-bound
thread pool. Useful for doing cpu-bound work, or when spawning a large
number of threads would be undesirable."
  `(bound-future-call (^{:once true} fn* [] ~@body) clojure.lang.Agent/pooledExecutor))