(ns programmingClojure.fp)

(defn stack-consuming-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (stack-consuming-fibo (- n 1))
             (stack-consuming-fibo (- n 2)))))
(stack-consuming-fibo 9)
(stack-consuming-fibo 99999)

(defn tail-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))]
    (fib 0N 1N n)))
(tail-fibo 9)
(tail-fibo 99999)

(defn recur-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n))))]
    (fib 0N 1N n)))

(recur-fibo 9)
(recur-fibo 1000000)

(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
      (cons n (lazy-seq-fibo b n))))))

(take 10 (lazy-seq-fibo))
(rem (nth (lazy-seq-fibo) 1000000) 1000)

(take 5 (iterate (fn [[a b]] [b (+ a b)]) [0 1]))

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(take 5 (fibo))

(def lots-o-fibs (take 1000000000 (fibo)))

(nth lots-o-fibs 100)

(set! *print-length* 10)
(take 1000000000 (fibo))
(fibo)

(def head-fibo (lazy-cat [0N 1N] (map + head-fibo (rest head-fibo))))
(take 10 head-fibo)
(nth head-fibo 1000000)

(defn count-heads-pairs [coll]
  (loop [cnt 0 coll coll]
    (if (empty? coll)
      cnt
      (recur (if (= :h (first coll) (second coll))
               (inc cnt)
               cnt)
             (rest coll)))))
(count-heads-pairs [:h :h :h :t :h])
(count-heads-pairs [:h :t :h :t :h])

(defn by-pairs [coll]
  (let [take-pair (fn [c]
                    (when (next c) (take 2 c)))]
    (lazy-seq
     (when-let [pair (seq (take-pair coll))]
       (cons pair (by-pairs (rest coll)))))))
(by-pairs [:h :t :t :h :h :h])

(defn count-heas-pairs [coll]
  (count (filter (fn [pair] (every? #(= :h %) pair))
                 (by-pairs coll))))
(partition 2 [:h :t :t :h :h :h])
(partition 2 1 [:h :t :t :h :h :h])

(def ^{:doc "Count items matching a filter"}
  count-if (comp count filter))

(count-if odd? [1 2 3 4 5])

(defn count-runs
  "Count runs of length n where pred is true in coll."
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))

(count-runs 2 #(= % :h) [:h :t :t :h :h :h])
(count-runs 2 #(= % :t) [:h :t :t :h :h :h])
(count-runs 3 #(= % :h) [:h :t :t :h :h :h])

(def ^{:doc "Count runs of length tow that are both heads"}
  count-heads-pairs (partial count-runs 2 #(= % :h)))

(defn faux-curry [& args] (apply partial partial args))

(def add-3 (partial + 3))
(add-3 7)

(def add-3 ((faux-curry +) 3))
(add-3 7)
