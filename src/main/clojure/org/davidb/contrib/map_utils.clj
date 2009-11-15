(ns org.davidb.contrib.map-utils)

(defmacro let-map
  "Evaluates the exprs, each of which should return a sequence.  These
  are bound to the binding forms for subsequent calls, and the results of
  the last expression in body are collected into a sequence."
  [bindings & body]
  (assert (vector? bindings))
  (assert (even? (count bindings)))
  (let [pairs (partition 2 bindings)
        bs (map first pairs)
        exprs (map second pairs)]
    `(map (fn [~@bs] ~@body) ~@exprs)))
