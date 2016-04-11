(ns chapter6)

(defprotocol Matrix
  "Protocol for working with 2d datastructures."
  (lookup [matrix i j])
  (update [matrix i j value])
  (rows [matrix])
  (cols [matrix])
  (dims [matrix]))

(extend-protocol Matrix
  clojure.lang.IPersistentVector
  (lookup [vov i j]
    (get-in vov [i j]))
  (update [vov i j value]
    (assoc-in vov [i j] value))
  (rows [vov]
    (seq vov))
  (cols [vov]
    (apply map vector vov))
  (dims [vov]
    [(count vov) (count (first vov))]))

(defn vov
  "Create a vector of h w-item vectors."
  [h w]
  (vec (repeat h (vec (repeat w nil)))))

(def matrix (vov 3 4))

matrix

(update matrix 1 2 :x)

(lookup *1 1 2)

(rows (update matrix 1 2 :x))
(cols (update matrix 1 2 :x))

(extend-protocol Matrix
  (Class/forName "[[D")
  (lookup [matrix i j]
    (aget matrix i j))
  (update [matrix i j value]
    (let [clone (aclone matrix)]
      (aset clone i
            (doto (aclone (aget clone i))
              (aset j value)))
      clone))
  (rows [matrix]
    (map vec matrix))
  (cols [matrix]
    (apply map vector matrix))
  (dims [matrix]
    (let [rs (count matrix)]
      (if (zero? rs)
        [0 0]
        [rs (count (aget matrix 0))]))))

(def matrix (make-array Double/TYPE 2 3))

(rows matrix)

(rows (update matrix 1 1 3.4))

(lookup (update matrix 1 1 3.4) 1 1)

(cols (update matrix 1 1 3.4))

(dims matrix)

(defrecord Point [x y])
(deftype Point [x y])

(Point. 3 4)
(.x (Point. 3 4))

(defrecord NamedPoint [^String name ^long x ^long y])

(NamedPoint/getBasis)

(map meta (NamedPoint/getBasis))

(def x "hello")

(defrecord Point [x y])

(Point. 5 5)

(ns user2)
(refer 'chapter6)
x
Point
(import 'chapter6.Point)
Point

(ns chapter6)
(defrecord Point [x y])

(= (Point. 3 4) (Point. 3 4))

(= 3 3N)

(= (Point. 3N 4N) (Point. 3 4))

(:x (Point. 3 4))

(:z (Point. 3 4) 0)

(map :x [(Point. 3 4)
         (Point. 5 6)
         (Point. 7 8)
         ])

(assoc (Point. 3 4) :z 5)

(let [p (assoc (Point. 3 4) :z 5)]
  (dissoc p :x))

(let  [p (assoc (Point. 3 4) :z 5)]
  (dissoc p :z))

(:z (assoc (Point. 3 4) :z 5))
(.z (assoc (Point. 3 4) :z 5))
(.x (assoc (Point. 3 4) :z 5))

(-> (Point. 3 4)
    (with-meta {:foo :bar})
    meta)
(->Point 3 4)

(map->Point {:x 3, :y 4, :z 5})

(apply ->Point [5 6])

(map (partial apply ->Point) [[5 6] [7 8] [9 10]])

(map map->Point [{:x 1 :y 2} {:x 5 :y 6 :z 44}])

(Point/create {:x 3, :y 4, :z 5})

(defn log-point
  [x]
  {:pre [(pos? x)]}
  (Point. x (Math/log x)))

(log-point -42)

(log-point Math/E)

(defn point [x y]
  {:x x, :y y})

(defrecord Point [x y])

(= (Point. 3 4) (Point. 3 4))

(= {:x 3 :y 4} (Point. 3 4))
(= (Point. 3 4) {:x 3 :y 4})

(deftype Point [x y])

(.x (Point. 3 4))

(:x (Point. 3 4))

(deftype SchrodingerCat [^:unsynchronized-mutable state]
  clojure.lang.IDeref
  (deref [sc]
    (locking sc
      (or state
          (set! state (if (zero? (rand-int 2))
                        :dead
                        :alive))))))

(defn schrodinger-cat
  "Creates a new Schrodinger's cat. Beware, the PEPL may kill it!"
  []
  (SchrodingerCat. nil))

(def felix (schrodinger-cat))

@felix

(schrodinger-cat)
(schrodinger-cat)
