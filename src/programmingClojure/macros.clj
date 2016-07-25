(ns programmingClojure.macros
  (:use clojure.set))

(defn unless [expr form]
  (if expr nil form))

(unless false (println "this should print"))
(unless true (println "this should not print"))

(defn unless [expr form]
  (println "About to test...")
  (if expr nil form))

(defmacro unless [expr form]
  (list 'if expr nil form))

(unless false (println "this should print"))
(unless true (println "this should not print"))

(macroexpand-1 '(bad-unless false (println "this should print")))

(.. arm getHand getFinger)
(macroexpand '(.. arm getHand getFinger))

(and 1 2 3)
(macroexpand '(and 1 2 3))

(unless false (println "this") (println "and also this"))

(when-not false (println "this") (println "and also this"))

; chain reimplemments Clojures's .. macro
(defmacro chain [x form]
  (list '. x form))

(defmacro chain
  ([x form] (list '. x form))
  ([x form & more] (concat (list 'chain (list '. x form)) more)))

(macroexpand '(chain arm getHand))
(macroexpand '(chain arm getHand getFinger))

(defmacro chain [x form]
  `(. ~x ~form))

(macroexpand '(chain arm getHand))

(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~more)))

(macroexpand '(chain arm getHand getFinger))

(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

(macroexpand '(chain arm getHand))
(macroexpand '(chain arm getHand getFinger))

(time (str "a" "b"))

; (bench (str "a" "b"))
; should expand to
(let [start (System/nanoTime)
      result (str "a" "b")]
  {:result result :elapsed (- (System/nanoTime) start)})

(defmacro bench [expr]
  `(let [start (System/nanoTime)
         result ~expr]
  {:result result :elapsed (- (System/nanoTime) start)}))

(bench (str "a" "b"))
(macroexpand-1 '(bench (str "a" "b")))

(let [a 1 b 2]
  (bench (+ a b)))

`foo#

(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
  {:result result# :elapsed (- (System/nanoTime) start#)}))

(bench (str "a" "b"))
(macroexpand-1 '(bench (str "a" "b")))

(and 1 2)

(defmacro testor
  ([] nil)
  ([x] x)
  ([x & rest]
   `(let [testor# ~x]
      (if testor# testor# (testor ~@rest)))))

(defmacro defstruct
  [name & keys]
  `(def ~name (create-struct ~@keys)))

(defmacro declare
  [& names] `(do ~@(map #(list 'def %) names)))

(#(list 'def %) 'a)

(map #(list 'def %) '[a b c d])
`(do ~@(map #(list 'def %) '[a b c d]))

(defmacro import-static
  "Imports the named static fields and/or static methods of the class
  as (private) symbols in the current namespace.

  Example:
      user=> (import-static java.lang.Math PI sqrt)
      nil
      user=> PI
      3.141592653589793
      user=> (sqrt 16)
      4.0

  Note: The class name must be fully qualified, even if it has already
  been imported.  Static methods are defined as MACROS, not
  first-class fns."
  [class & fields-and-methods]
  (let [only (set (map str fields-and-methods))
        the-class (. Class forName (str class))
        static? (fn [x]
                    (. java.lang.reflect.Modifier
                       (isStatic (. x (getModifiers)))))
        statics (fn [array]
                    (set (map (memfn getName)
                              (filter static? array))))
        all-fields (statics (. the-class (getFields)))
        all-methods (statics (. the-class (getMethods)))
        fields-to-do (intersection all-fields only)
        methods-to-do (intersection all-methods only)
        make-sym (fn [string]
                     (with-meta (symbol string) {:private true}))
        import-field (fn [name]
                         (list 'def (make-sym name)
                               (list '. class (symbol name))))
        import-method (fn [name]
                          (list 'defmacro (make-sym name)
                                '[& args]
                                (list 'list ''. (list 'quote class)
                                      (list 'apply 'list
                                            (list 'quote (symbol name))
                                            'args))))]
    `(do ~@(map import-field fields-to-do)
         ~@(map import-method methods-to-do))))

(import-static java.lang.Math PI pow)
PI
(pow 10 3)

(def slow-calc (delay (Thread/sleep 5000) "done!"))

(force slow-calc)

(with-out-str (print "hello, ") (print "world"))

(defmacro with-out-str
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))
(defmacro bad-unless [expr form]
  (list 'if 'expr nil form))

(assert (= 1 1))

(assert (= 1 2))

(defn bench-fn [f]
  (let [start (System/nanoTime)
        result (f)]
    {:result result :elapsed (- (System/nanoTime) start)}))

(bench (+ 1 2))
(bench-fn (fn [] (+ 1 2)))
