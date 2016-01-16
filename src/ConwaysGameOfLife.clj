(ns ConwaysGameOfLife)
(use 'clojure.pprint)
(defn empty-board
  "Creates a rectangular empty board of the specified width
  and height"
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate 
  "Turns :on each of the cells specified as [y, x] coordinates."
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))
(pprint glider)

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))
(neighbours [1 1])
(not= 0 1 0)
(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn step 
  "Yields the next state of the world"
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
            :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})
     (drop 8)
     first
     (populate (empty-board 6 6))
     pprint)
(->> [1 2 3] (map str) )

(frequencies (mapcat neighbours #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(let [cells #{[2 0] [2 1] [2 2] [1 2] [0 1]}]
  (set (for [[loc n] (frequencies (mapcat neighbours cells)) 
    :when (or (= n 3) (and (= n 2) (cells loc)))] 
         loc)))

(let [cells #{[2 0] [2 1] [2 2] [1 2] [0 1]}]
  (step cells))
(take 20 (for [x (range 10) y (range 10) :while (> x y)] [x y]))

(defn stepper
  "Returns a step function for Life-like cell automata.
  neighbours takes a location and return a sequential collection
  of locations. survive? and birth? are predicates on the number
  of living neighbours."
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells)) 
               :when (if (cells loc) (survive? n) (birth? n))] 
         loc))))

(defn hex-neighbours
  [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])]
    [(+ dx x) (+ dy y)]))

(hex-neighbours [0 0])

(def hex-step (stepper hex-neighbours #{2} #{3 4}))
(hex-step #{[0 0] [1 1] [1 3] [0 4]})
(hex-step *1)
(hex-step *1)
(hex-step *1)

;(stepper #(filter (fn [[i j]] (and (< -1 i w) (< -1 j h)))
;                  (neighbours %)) #{2 3} #{3})
