(ns WilsonMap)
(defn maze
  "Returns a random maze carved out of walls; walls is a set of
  2-item sets #{a b} where a and b are locations.
  The returned maze is a set of the remaining walls."
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))

(loop [x 5]
  (if (neg? x)
    x
    (recur (dec x))))

(defn countdown
  [x]
  (if (zero? x)
    :blastoff!
    (do (println x)
        (recur (dec x)))))

(countdown 5)

(defn grid
  [w h]
  (set (concat
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(grid 2 2)

(defn draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [g (doto ^java.awt.Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc))))))
        (.setPreferredSize (java.awt.Dimension.
                             (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(draw 3 3 (maze (grid 3 3)))
(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))
(java.awt.BasicStroke. 0.4)

(def frame (javax.swing.JFrame. "Hello Frame"))
(.setSize frame 200 200)
(.setVisible frame true)
(defn wmaze
  "Returns a random maze carved out of walls; walls is a set of
  2-item sets #{a b} where a and b are locations.
  The returned maze is a set of the remaining walls."
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))
              walk (take-while identity (iterate steps loc))
              steps (zipmap walk (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))

(draw 3 3 (wmaze (grid 3 3)))

(defn hex-grid
  [w h]
  (let [verticles (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)]
                         [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v verticles d deltas f [+ -]
               :let [w (verticles (map f v d))]
               :when w] #{v w}))))

(defn- hex-outer-walls
  [w h]
  (let [verticles (set (for [y (range h) x (range (if (odd? y) 1 0) (* 2 w) 2)]
                         [x y]))
        deltas [[2 0] [1 1] [-1 1]]]
    (set (for [v verticles d deltas f [+ -]
               :let [w (map f v d)]
               :when (not (verticles w))] #{v (vec w)}))))

(defn hex-draw
  [w h maze]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [maze (into maze (hex-outer-walls w h))
                      g (doto ^java.awt.Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4
                                                             java.awt.BasicStroke/CAP_ROUND
                                                             java.awt.BasicStroke/JOIN_MITER)))
                      draw-line (fn [[[xa ya] [xb yb]]]
                                  (.draw g
                                         (java.awt.geom.Line2D$Double.
                                           xa (* 2 ya) xb (* 2 yb))))]
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (draw-line
                      (cond
                        (= ya yb) [[(inc xa) (+ ya 0.4)] [(inc xa) (- ya 0.4)]]
                        (< ya yb) [[(inc xa) (+ ya 0.4)] [xa (+ ya 0.6)]]
                        :else [[(inc xa) (- ya 0.4)] [xa (- ya 0.6)]]))))))
        (.setPreferredSize (java.awt.Dimension.
                             (* 20 (inc w)) (* 20 (+ 0.5 h))))))
    .pack
    (.setVisible true)))


(hex-draw 20 20 (maze (hex-grid 20 20)))
(hex-draw 2 2 (hex-grid 2 2))
(hex-draw 2 2 (hex-outer-walls 2 2))

(require '[clojure.zip :as z])
(def v [[1 2 [3 4]] [5 6]])
(-> v z/vector-zip z/node)
(-> v z/vector-zip z/down z/node)
(-> v z/vector-zip z/down z/right z/node)
(-> v z/vector-zip z/down z/right (z/replace 56) z/node)
(-> v z/vector-zip z/down z/right (z/replace 56) z/root)
(-> v z/vector-zip z/down z/right z/remove z/node)
(-> v z/vector-zip z/down z/right z/remove z/root)
(-> v z/vector-zip z/down z/down z/right (z/edit * 42) z/root)

(defn html-zip [root]
  (z/zipper
    vector?
    (fn [[tagname & xs]]
      (if (map? (first xs)) (next xs) xs))
    (fn [[tagname & xs] children]
      (into (if (map? (first xs)) [tagname (first xs)] [tagname])
            children))
    root))

(defn wrap 
  "Wraps the current node in the specified tag and attributes."
  ([loc tag]
   (z/edit loc #(vector tag %)))
  ([loc tag attrs] 
   (z/edit loc #(vector tag attrs %))))
(def h [:body [:h1 "Clojure"]
        [:p "What a wonderful language!"]])

(-> h html-zip z/down z/right z/down (wrap :b) z/root)

(def labyrinith (maze (grid 10 10)))
(def labyrinith (let [g (grid 10 10)] (reduce disj g (maze g))))
(def theseus (rand-nth (distinct (apply concat labyrinith))))
(def minotaur (rand-nth (distinct (apply concat labyrinith))))

(defn ariadne-zip
  [labyrinith loc]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq labyrinith))
        children (fn [[from to]]
                   (seq (for [loc (paths to)
                              :when (not= loc from)]
                          [to loc])))]
    (z/zipper (constantly true)
              children
              nil
              [nil loc])))

(->> theseus
     (ariadne-zip labyrinith)
     (iterate z/next)
     (filter #(= minotaur (second (z/node %))))
     first z/path
     (map second))


(defn drawMazePath
  [w h maze path]
  (doto (javax.swing.JFrame. "Maze")
    (.setContentPane
      (doto (proxy [javax.swing.JPanel] []
              (paintComponent [^java.awt.Graphics g]
                (let [g (doto ^java.awt.Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (java.awt.BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc)))
                  (.translate g -0.5 -0.5)
                  (.setColor g java.awt.Color/RED)
                  (doseq [[[xa ya] [xb yb]] path]
                    (.drawLine g xa ya xb yb)))))
        (.setPreferredSize (java.awt.Dimension.
                             (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(let [w 40, h 40
      grid (grid w h)
      walls (maze grid)
      labyrinith (reduce disj grid walls)
      places (distinct (apply concat labyrinith))
      theseus (rand-nth places)
      minotaur (rand-nth places)
      path (->> theseus
                (ariadne-zip labyrinith)
                (iterate z/next)
                (filter #(= minotaur (first (z/node %))))
                first z/path rest)]
  (drawMazePath w h walls path))
