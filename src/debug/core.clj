(ns debug.core
  (:gen-class))

(defn repl []
  (let [input (read)]
    (if (= input 'q)
      nil
      (do
        (let [result (eval input)]
          (println result)
          (recur))))))

(defn dispatch-form [form]
  (first form))

(defmulti debug-form dispatch-form)


(defmethod debug-form :default
  [form]
  (println form)
  `(do
     (repl)
     ~form))

(defmethod debug-form 'let
  [[_ binds body]]
  (let [syms (take-nth 2 binds)
        vals (take-nth 2 (rest binds))
        vals-dbg (map debug-form vals)
        binds* (vec (interleave syms vals-dbg))
        ]
    (list 'let binds* body)
    ))

(defn bar [pairs]
  (if (empty? pairs)
    nil
    (let [head (first pairs)
          tail (rest pairs)]
      (list 'let [head] (bar tail)))))

(defmacro debug [& body]
  (cons
   'do
   (for [form body]
     (debug-form form)
     )

        )
  )
