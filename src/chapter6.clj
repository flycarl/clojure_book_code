(ns chapter6)
*ns*
(defn a [] 42)
(in-ns 'physics.constants)
*ns*
(def ^:const planck 6.62606957e-34)

(clojure.core/+ 1 1)
(clojure.core/range -20 20 4)

chapter6/a
(clojure.core/refer 'chapter6)
(a)

(clojure.core/refer 'clojure.core
                    :exclude '(range)
                    :rename '{+ add
                              - sub
                              / div
                              * mul})

(-> 5 (add 18) (mul 2) (sub 6))

;(range -20 20 4)

;start new repl
(require 'clojure.set)

(clojure.set/union #{1 2 3} #{4 5 6})

(require '[clojure.set :as set])

(set/union #{1 2 3} #{4 5 6})

(require '(clojure string [set :as set]))

(use '(clojure [string :only (join) :as str]
               [set :exclude (join)]))

join

intersection

str/trim

;(Date.)

(java.util.Date.)

(import 'java.util.Date 'java.text.SimpleDateFormat)

(.format (SimpleDateFormat. "MM/dd/yyyy") (Date.))

(import '(java.util Arrays Collections))

(->> (iterate inc 0)
     (take 5)
     into-array
     Arrays/asList
     Collections/max)

;(import 'java.awt.List 'java.util.List)
;= List already refers to: class

(in-ns 'exmaples.ns)
(clojure.core/refer 'clojure.core :exclude '[next replace remove])
(require '(clojure [string :as string]
                   [set :as set])
         '[clojure.java.shell :as sh])
(use '(clojure zip xml))
(import 'java.util.Date
        'java.text.SimpleDateFormat
        '(java.util.concurrent Executors
                               LinkedBlockingQueue))

(ns examples.ns
  (:refer-clojure :exclude [next replace remove])
  (:require (clojure [string :as string]
                     [set :as set])
            [clojure.java.shell :as sh])
  (:use (clojure zip xml))
  (:import java.util.Date
           java.text.SimpleDateFormat
           (java.util.concurrent Executors
                                 LinkedBlockingQueue)))
