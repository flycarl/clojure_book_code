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
  (time (count s)))

(cons 0 (range 1 5))
(cons 0 (cons 1 (cons 2 (cons 3 (range 4 10)))))
(list* 0 1 2 3 4 (range 4 10))

(defn random-ints
  "Return a lazy seq of drandom intergers in the range [0,limit)."
  [limit]
  (lazy-seq
    (cons (rand-int limit)
          (random-ints limt))))

