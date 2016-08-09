(ns clojurebreaker.snippets)

(defn exact-matches
  "Given two collections, return the number of positions where
  the collections contain equal items."
  [c1 c2])

(require '[clojure.data :as data])
(data/diff [:r :g :g :b] [:r :y :y :b])

(defn exact-matches
  "Given two collections, return the number of positions where
  the collections contain equal items."
  [c1 c2]
  (let [[_ _ matches] (data/diff c1 c2)]
    (count (remove nil? matches))))

(exact-matches [:r :g :g :b] [:r :y :y :b])

(def example-secret [:r :g :g :b])
(frequencies example-secret)

(def example-guess [:y :y :y :g])
(frequencies example-guess)

(select-keys (frequencies example-secret) example-guess)
(select-keys (frequencies example-guess) example-secret)

(merge-with min {:g 1} {:g 2})

(defn unordered-matches
  "Given two collections, return a map where each key is an item
  in both collections, and each value is the number of times the
  value occurs in the collection with fewest occurrences."
  [c1 c2]
  (let [f1 (select-keys (frequencies c1) c2)
        f2 (select-keys (frequencies c2) c1)]
    (merge-with min f1 f2)))

(unordered-matches [:r :g :g :b] [:r :y :y :g])

(defn score
  [c1 c2]
  (let [exact (exact-matches c1 c2)
        unordered (apply + (vals (unordered-matches c1 c2)))]
    {:exact exact :unordered (- unordered exact)}))

(score [:r :g :g :b] [:r :y :y :g])

(require '[clojure.math.combinatorics :as comb])
(comb/selections [:r :g :b] 2)

(-> (comb/selections [:r :g :b] 2)
    (comb/selections 2))

(defn generate-turn-inputs
  "Generate all possible turn inputs for a clojurebreaker game
  with colors and n columns"
  [colors n]
  (-> (comb/selections colors n)
      (comb/selections 2)))

(defn score-inputs
  "Given a sequence of turn inputs, return a lazy sequence of
  maps with :secret, :guess, and :score."
  [inputs]
  (map
   (fn [[secret guess]]
     {:secret (seq secret)
      :guess (seq guess)
      :score (score secret guess)})
   inputs))

(->> (generate-turn-inputs [:r :g :b] 2)
     (score-inputs))

(use 'clojure.pprint)
(require '[clojure.java.io :as io])
(with-open [w (io/writer "scoring-table")]
  (binding [*out* w]
    (print-table (->> (generate-turn-inputs [:r :g :b :y] 4)
                      (score-inputs)))))

( '[clojure.test.generative.generators :as gen])
(gen/int)
(gen/char)
(gen/boolean)
(gen/vec gen/boolean)
(gen/hash-map gen/byte gen/int)
(gen/geometric 0.02)
(gen/list gen/int 2)
(gen/list gen/int (gen/uniform 0 5))

(defn random-secret
  []
  (gen/vec #(gen/one-of :r :g :b :y) 4))

(random-secret)
