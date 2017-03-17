(ns debug.core
  (:gen-class))

(defmacro repl* []
  `((fn [] ~(read))))

;; (defn repl2 []
;;   (let [body (read)]
;;     (fn [] )
;;     )
;;   `((fn [] ~(read))))

;; (defn repl3 []
;;   (repl*))

(defn repl []
  (let [input (read)]
    (if (= input 'q)
      nil
      (do
        (let [result (eval input)]
          (println result)
          (recur))))))

(def repl-code
  `(loop []
     (let [input# (read)]
       (if (= input# :q)
         nil
         (let [result# (deref (delay input#))]
           (println result#)
           (recur))))))

(defn dispatch-form [form]
  (first form))

(defmulti debug-form dispatch-form)

(defmethod debug-form :default
  [form]
  `(do
     (println (quote ~form))
     ;; (flush)
     ;; (repl3)
     (repl*)
     ;; (read)
     ~form))

(defmethod debug-form 'let
  [[_ binds body]]

  (let [syms (take-nth 2 binds)
        vals (take-nth 2 (rest binds))
        vals-dbg (map debug-form vals)
        binds* (vec (interleave syms vals-dbg))]
    (list 'let binds* body)))

(defn with-repl [form]
  (println form)
  (repl)
  form)

;; (defn bar [binds body]
;;   (if (empty? binds)
;;     body
;;     (let [[sym form & tail] binds]
;;       (list 'let [sym (list 'do repl-code form)] (bar tail body)))))

(defmacro debug [& body]
  (cons
   'do
   (for [form body]
     (debug-form form))))
