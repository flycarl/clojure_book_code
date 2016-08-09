(ns programmingClojure.multimethods)
(require Clojure.string)

(defn my-print [ob]
  (.write *out* ob))

(defn my-println [ob]
  (my-print ob)
  (.write *out* "\n"))

(my-println "hello")
(my-println nil)


(defn my-print [ob]
  (cond
    (nil? ob) (.write *out* "nil")
    (string? ob) (.write *out* ob)))

(my-println nil)

(my-println [1 2 3])

(require '[clojure.string :as str])
(defn my-print-vector [ob]
  (.write *out* "[")
  (.write *out* (str/join " " ob))
  (.write *out* "]"))
(defn my-print [ob]
  (cond
    (vector? ob) (my-print-vector ob)
    (nil? ob) (.write *out* "nil")
    (string? ob) (.write *out* ob)))

(my-println [1 2 3])

(defmulti my-print class)

(my-println "foo")

(defmethod my-print String [s]
  (.write *out* s))

(my-println "stu")

(defmethod my-print nil [s]
  (.write *out* "nil"))

(my-println nil)

(defmethod my-print Number [n]
  (.write *out* (.toString n)))

(my-println 43)
(isa? Integer Number)

(defmethod my-print :default [s]
  (.write *out* "#<")
  (.write *out* (.toString s))
  (.write *out* ">"))

(my-println (java.sql.Date. 0))
(my-println (java.util.Random.))

(defmulti my-print class :default :everything-else)
(defmethod my-print String [s]
  (.write *out* s))
(defmethod my-print :everything-else [_]
  (.write *out* "Not implemented yet..."))

(require '[clojure.string :as str])
(defmethod my-print java.util.Collection [c]
  (.write *out* "(")
  (.write *out* (str/join " " c))
  (.write *out* ")"))

(my-println (take 6 (cycle [1 2 3])))
(my-println [1 2 3])

(defmethod my-print clojure.lang.IPersistentVector [c]
  (.write *out* "[")
  (.write *out* (str/join " " c))
  (.write *out* "]"))

(my-println [1 2 3])

(prefer-method
 my-print clojure.lang.IPersistentVector java.util.Collection)

(my-println (take 6 (cycle [1 2 3])))
(my-println [1 2 3])
(print)
