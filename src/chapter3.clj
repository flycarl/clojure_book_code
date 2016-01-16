(ns chapter3)

(def v [1 2 3])
(conj v 4)
(conj v 4 5)
(seq v)

(def m {:a 5 :b 6})
(conj m [:c 7])
(seq m)

(def s #{1 2 3})
(conj s 10)
(conj s 3 4)
(seq s)

(def lst '(1 2 3))
(conj lst 0)
(conj lst 0 -1)
(seq lst)

(into v [4 5])
(into '(1 2 3) [:a :b :c])

(defn swap-pairs
  [sequential]
  (into (empty sequential)
        (interleave
          (take-nth 2 (drop 1 sequential))
          (take-nth 2 sequential))))

(swap-pairs (apply list (range 10)))
(swap-pairs (apply vector (range 10)))

(defn map-map
  [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))
(map-map inc (hash-map :z 5 :c 6 :a 0))
(map-map inc (sorted-map :z 5 :c 6 :a 0))

(seq "clojure")
(seq {:a 5 :b 6})
(seq (java.util.ArrayList. (range 5)))
(seq (into-array ["clojure" "Programming"]))
(seq [])
(seq nil)

(map str "CLojure")
(set "Programming")

(let [r (range 3)
      rst (rest r)]
  (prn (map str rst))
  (prn (map #(+ 100 %) r))
  (prn (conj r -1) (conj rst 42)))

(let [s (range 1e6)]
  (time (count s)))
(let [s (apply list (range 1e6))]
  (time (count s)))1.8.0_60-b27

(cons 0 (range 1 5))
(cons 0 (cons 1 (cons 2 (cons 3 (range 4 10)))))
(list* 0 1 2 3 4 (range 4 10))

(defn random-ints
  "Return a lazy seq of drandom intergers in the range [0,limit)."
  [limit]
  (lazy-seq
    (cons (rand-int limit)
          (random-ints limit))))

(def rands (take 10 (random-ints 50)))
(first rands)
(nth rands 3)
(count rands)
(count rands)

(repeatedly 10 (partial rand-int 50))

(apply str (remove (set "aeiouy")
                   "vowels are uselese! or maybe not ..."))

(defn magnitude
  [x]
  (-> x Math/log10 Math/floor))

(magnitude 100)
(magnitude 1000)
(magnitude 75)
(defn compare-magnitude
  [a b]
  (- (magnitude a) (magnitude b)))


((comparator compare-magnitude) 10 75)
((comparator compare-magnitude) 0 0)


(compare-magnitude 10 75)

(defn interpolate
  [points]
  (let [results (into (sorted-map) (map vec points))]
    (fn [x]
      (let [[xa ya] (first (rsubseq results <= x))
            [xb yb] (first (subseq results > x))]
        (if (and xa xb)
          (/ (+ (* ya (- xb x)) (* yb (- x xa)))
             (- xb xa))
          (or ya yb))))))


(def f (interpolate [[0 0] [10 10] [15 5]]))
(map f [2 10 12])

(get [:a :b :c] 2)
(get {:a 5 :b 6} :b)
(get {:a 5 :b 6} :c 7)
(get #{1 2 3} 3)


([:a :b :c] 2)
({:a 5 :b 6} :b)
({:a 5 :b 6} :c 7)
(#{1 2 3} 3)

(:b {:a 5 :b 6})

(defn get-foo
  [map]
  (:foo map))
(get-foo nil)

(defn get-bar
  [map]
  (map :bar))

(get-bar nil)

(map :name [{:age 21 :name "David"}    
            {:gender :f :name "Suzanne"}    
            {:location "NYC" :name "Sara"}])    
(filter :age [{:age 21 :name "David"}    
              {:gender :f :name "Suzanne"}    
              {:location "NYC" :name "Sara"}])
(filter (comp (partial <= 25) :age) [{:age 21 :name "David"}    
                                     {:gender :f :name "Suzanne" :age 35}    
                                     {:location "NYC" :name "Sara" :age 20}])


(def a {:a 5 :b 6 :c 7 :d 8})

(into #{} (range 5))
(defn native-into
  [coll source]
  (reduce conj coll source))
(= (into #{} (range 500))
   (native-into #{} (range 500)))
(time (do (into #{} (range 1e6)) nil))
(time (do (native-into #{} (range 1e6)) nil))


