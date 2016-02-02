(ns chapter4 (:use clojure.pprint))

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

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))
(swap! sarah update-in [:age] + 3)
(swap! sarah (comp #(update-in % [:age] inc)
                   #(assoc % :wears-glasses? false)))

(def xs (atom #{1 2 3}))
(wait-futures 1 (swap! xs (fn [v]
                            (Thread/sleep 250)
                            (println "trying 4")
                            (conj v 4)))

              (swap! xs (fn [v]
                          (Thread/sleep 500)
                          (println "trying 5")
                          (conj v 5))))

(def x (atom 2000))
(swap! x #(Thread/sleep %))

(compare-and-set! xs :wrong "new value")
(compare-and-set! xs @xs "new value")

(def xs (atom #{1 2}))
(compare-and-set! xs #{1 2} "new value")
(reset! xs :y)
(deref xs)

(defn echo-watch
  [key identity old new]
  (println key old "=>" new))

(def sarah (atom {:name "Sarah" :age 25}))
(add-watch sarah :echo echo-watch)
(swap! sarah update-in [:age] inc)
(add-watch sarah :echo2 echo-watch)
(swap! sarah update-in [:age] inc)

(remove-watch sarah :echo2)
(swap! sarah update-in [:age] inc)

(def history (atom ()))

(defn log->list
  [dest-atom key source old new]
  (when (not= old new)
    (swap! dest-atom conj new)))

(def sarah (atom {:anme "Sarah", :age 25}))
(add-watch sarah :record (partial log->list history))
(swap! sarah update-in [:age] inc)
(swap! sarah update-in [:age] inc)
(swap! sarah identity)
(swap! sarah assoc :wears-glasses? true)
(swap! sarah update-in [:age] inc)

(pprint (deref history))


(def n (atom 1 :validator pos?))
(swap! n + 500)
(swap! n - 1000)

(set-validator! sarah #(or (:age %)
                           (throw (IllegalStateException. "People must have `:age`s!"))))
(swap! sarah dissoc :age)

(defn unsafe
  []
  (io! (println "writing to database...")))
(dosync (unsafe))

(def x (ref (java.util.ArrayList.)))
(wait-futures 2 (dosync (dotimes [v 5]
                          (Thread/sleep (rand-int 50))
                          (alter x #(doto % (.add v))))))
(deref x)

(def x (ref 0))
(dosync 
  @(future (dosync (ref-set x 0)))
  (ref-set x 1))

(def a (ref 0))
(future (dotimes [_ 500] (dosync (Thread/sleep 200) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

(def a (ref 0))
(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

(def a (ref 0 :max-history 100))
(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

(def a (ref 0 :min-history 50 :max-history 100))
(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

map
#'map
@#'map

(def ^:private everyting 42)
(def ^{ :private true} everyting 42)

(def a
  "A sample value."
  5)

(defn b 
  "A sample calculation using `a`." 
  [c]
  (+ a c))
(b 3)
(doc b)

(let [a 1
      b 2]
  (println (+ a b))
  (let [b 3
        + -]
    (println (+ a b))))

(def ^:dynamic *max-value* 255)
(defn valid-value?
  [v]
  (<= v *max-value*))
(binding [*max-value* 500]
  (valid-value? 299))
(binding [*max-value* 500]
  (println (valid-value? 299))
  (doto (Thread. #(println "in other thread:" (valid-value? 299)))
    .start
    .join))

(def ^:dynamic *var* :root)
(defn get-*var* [] *var*)
(binding [*var* :a]
  (binding [*var* :b]
    (binding [*var* :c]
      (get-*var*))))

(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (if (== 404 response-code)
      [response-code]
      [response-code (-> conn .getInputStream slurp)])))
(http-get "http://163.com/bac-url")
(http-get "http://163.com/")
