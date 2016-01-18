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
