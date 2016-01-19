(ns chapter4)

(def d (delay (println "Running...")
              :done!))
(deref d)

(defn get-document
  [id]
  ; ... do some work to retrieve the identified document's metadata ...
  {:url "https://www.mozilla.org/en-US/about/manifesto/"
   :title "The Mozilla Manifesto"
   :mime "text/html"
   :content (delay (slurp "https://www.mozilla.org/en-US/about/manifesto/"))})
(def d (get-document "some-id"))

(realized? (:content d))
@(:content d)
(deref (:content d))
(realized? (:content d))

(def long-calculation (future (apply + (range 1e8))))
@long-calculation

@(future (Thread/sleep 1000) :done!)
(deref (future (Thread/sleep 5000) :done!)
       1000 
       :impatient!)
(defn get-document-in-future
  [id]
  ; ... do some work to retrieve the identified document's metadata ...
  {:url "https://www.mozilla.org/en-US/about/manifesto/"
   :title "The Mozilla Manifesto"
   :mime "text/html"
   :content (future (slurp "https://www.mozilla.org/en-US/about/manifesto/"))})

(def e (get-document-in-future "some-id"))

(realized? (:content e))
@(:content e)
(deref (:content e))

(def p (promise))
(realized? p)
(deliver p 42)
@p

(def a (promise))
(def b (promise))
(def c (promise))
(future (deliver c ( + @a @b))
        (println "Delivery complete!"))
(deliver a 15)
(deliver b 16)
@c

(deliver p @p)

(def aa (promise))
(def bb (promise))
(future (deliver aa @bb))
(future (deliver bb @aa))
(realized? aa)
(realized? bb)
(deliver aa 42)
@aa
@bb

(defn call-service
  [arg1 arg2 callback-fn]
  ; ... perfomr service call, eventually invoking callback-fn with results...
  (future (callback-fn (+ arg1 arg2) (- arg1 arg2))))

(defn sync-fn
  [async-fn]
  (fn [& args]
    (let [result (promise)]
      (apply async-fn (conj (vec args) #(deliver result %&)))
      @result)))

((sync-fn call-service) 8 7)

(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))

(phone-numbers "Sunil: 342.234.2342, Betty: 343.442.2342")
(apply str (concat (repeat 2 \space) "Sunil: 342.234.2342, Betty: 343.442.2342"))
(def files (repeat 100
                   (apply str
                          (concat (repeat 1000000 \space)
                                  "Sunil: 342.234.2342, Betty: 343.442.2342"))))
(time (dorun (map phone-numbers files)))
(time (dorun (pmap phone-numbers files)))
(def files2 (repeat 100000
                   (apply str
                          (concat (repeat 1000 \space)
                                  "Sunil: 342.234.2342, Betty: 343.442.2342"))))
(time (dorun (map phone-numbers files2)))
(time (dorun (pmap phone-numbers files2)))
(time (->> files2
           (partition-all 250)
           (pmap (fn [chunk] (doall (map phone-numbers chunk))))
           (apply concat)
           dorun))
