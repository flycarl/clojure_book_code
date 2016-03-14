(ns chapter5)
(require '(clojure [string :as str]
                   [walk :as walk]
                   [pprint as pprint]))
(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))

(reverse-it
 (qesod [gra (egnar 6)]
        (nltnirp (cni gra))))

(macroexpand-1 '(reverse-it
                 (qesod [gra (egnar 5)]
                        (nltnirp (cni gra)))))

(defn oops [args] (frobnicate arg))

(defmacro oopsMacro [arg] `(frobnicate ~arg))
(oopsMacro 123)

(macroexpand-1 '(oops 123))
(pprint (macroexpand '(reverse-it
                       (qesod [gra (egnar 5)]
                              (nltnirp (cni gra))))))

(macroexpand '(cond a b c d))

(walk/macroexpand-all '(cond a b c d))
(walk/macroexpand-all ''(when x a))

(defmacro hello
  [name]
  (list 'println name))

(macroexpand '(hello "Brain"))

(defmacro while
  [test & body]
  (list 'loop []
        (concat (list 'when test) body)
        '(recur)))

(defmacro while
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(def foo 123)
[foo (quote foo) 'foo `foo]

(in-ns 'bar)
`foo

(ns baz (:require [chapter5 :as c5]))
`map

`c5/foo

`foo
