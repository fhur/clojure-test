(ns test-macros.core)

;; times macro
(defmacro times
  "Given a number 'n' and a form 'code' as argument, outputs 'code' 'n' times"
  [n code]
  (cons `do
        (map (fn [_] code)
             (range n))))

(macroexpand-1 '(times 10 (println "Hello")))

(defmacro foreach
  "Given a symbol 'el' and a collection 'col', iterates over coll evaluating
  body on every step"
  [[el coll] body]
  `(loop [remaining# ~coll]
     (if (empty? remaining#)
       nil
       (let [~el (first remaining#)]
         ~body
         (recur (rest remaining#))))))

(foreach [x [1 2 3 4]] (println x))

(defmacro hygienic
  "Example that shows macro hgygiene"
  [& body]
  `(let [x# :macro-value]
     ~@body))

(def x "my variable")
(hygienic (println x))
(macroexpand-1 '(hygienic (str "foo")))

(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks [~@ks]))))

(let [x 1
      y 2
      z 3]
  (spy-env)
  (+ x y z))

;; {x 1, y 2, z 3}
;; 6


(defmacro simplify
  [body]
  (let [locals (set (keys &env))
        flattened-symbols (flatten body)]
    (if (some locals flattened-symbols)
      body
      (do (println "Precomputing some values...")
          (list `quote (eval body))))))

(simplify (let [x [1 2 3]
                y [2 3 4]]
            (concat x y)))

;; Precomputing some values...
;; (1 2 3 2 3 4)
