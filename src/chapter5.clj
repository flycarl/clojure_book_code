(ns chapter5 (:use clojure.pprint))
(require '(clojure [string :as str]
                   [walk :as walk]))


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

                                        ;(defn oops [args] (frobnicate arg))

(defmacro oopsMacro [arg] `(frobnicate ~arg))
                                        ;(oopsMacro 123)

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
(def foo 123)
(list `map `println [foo])
`(map println [~foo])
`(map println ~[foo])

`(println ~(keyword (str foo)))

(let [defs '((def x 123)
             (def y 456))]
  (concat (list 'do) defs))

(let [defs '((def x 123)
             (def y 456))]
  `(do ~@defs))

(defmacro foo
  [& body]
  `(do-something ~@body))

(macroexpand-1 '(foo (doseq [x (range 5)]
                       (println x))
                     :done))
'`(map println ~[foo])

(defn fn-hello [x]
  (str "Hello, " x "!"))

(defmacro macro-hello [x]
  `(str "Hello, " ~x "!"))

(fn-hello "Brian")
(macro-hello "Brian")

(map fn-hello ["Brain" "Not Brain"])
;; following line can't take value of a macro
;(map macro-hello ["Brain" "Not Brain"])
(map #(macro-hello %) ["Brain" "Not Brain"])

(defmacro unhygienic
  [& body]
  `(let [x :oops]
     ~@body))
;; following line can't let qulified name : baz/x
; (unhygienic (println "x:" x))
(macroexpand-1 `(unhygienic (println "x:" x)))

(defmacro still-unhygienic
  [& body]
  `(let [~'x :oops]
     ~@body))
(still-unhygienic (println "x:" x))

(let [x :this-is-important]
  (still-unhygienic
   (println "x:" x)))

(gensym "sym")

(defmacro hygienic
  [& body]
  (let [sym (gensym)]
    `(let [~sym :macro-value]
       ~@body)))
(let [x :important-value]
  (hygienic
   (println "x:" x)))

(defmacro hygienic
  [& body]
  `(let [x# :macro-value]
     ~@body))

`(x# x#)
(defmacro auto-gensyms
  [& numbers]
  `(let [x# (rand-int 10)]
     (+ x# ~@numbers)))
(auto-gensyms 1 2 3 4 5)
(macroexpand-1 '(auto-gensyms 1 2 3 4 5))

[`x# `x#]

(defmacro our-doto [expr & forms]
  `(let [obj# ~expr]
     ~@(map (fn [[fn & args]]
              `(~fn obj# ~@args)) forms)
     obj#))
;; this our-doto is wrong
;; (our-doto "It words"
;;          (println "I can't believe it"))

(defmacro our-doto [expr & forms]
  (let [obj (gensym "obj")]
    `(let [~obj ~expr]
       ~@(map (fn [[f & args]]
                `(~f ~obj ~@args)) forms)
       ~obj)))

(defmacro our-doto [expr & forms]
  (let [obj (gensym "obj")]
    `(let [~obj ~expr]
       ~@(map (fn [[f & args]]
                (list* f obj args)) forms)
       ~obj)))
(our-doto "It words"
          (println "I can't believe it")
          (println "I still can't believe it"))
(defmacro with
  [name & body]
  `(let [~name 5]
     ~@body))
(with bar (+ 10 bar))
(with foo (+ 40 foo))

(defmacro spy [x]
  `(do
     (println "spied" '~x ~x)
     ~x))

(spy 2)

(spy (rand-int 10))
(macroexpand-1 '(spy (rand-int 10)))

(defmacro spy [x]
  `(let [x# ~x]
     (println "spied" '~x x#)
    x#))
(spy (rand-int 10))
(macroexpand-1 '(spy (rand-int 10)))

(defn spy-helper [expr value]
  (println expr value)
  value)

(defmacro spy [x]
  `(spy-helper '~x ~x))

(spy (rand-int 10))
(macroexpand-1 '(spy (rand-int 10)))

(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks [~@ks]))))

(let [x 1 y 2]
     (spy-env)
     (+ x y))

(defmacro simplify
  [expr]
  (let [locals (set (keys &env))]
    (if (some locals (flatten expr))
      expr
      (do
        (println "Precomputing: " expr)
        (list `quote (eval expr))))))

(defn f
  [a b c]
  (+ a b c (simplify (apply + (range 5e7)))))

(f 1 2 3)

(defn f'
  [a b c]
  (simplify (apply + a b c (range 5e7))))
(f' 1 2 3)

(@#'simplify nil {} '(inc 1))
(@#'simplify nil {'x nil} '(inc x))
