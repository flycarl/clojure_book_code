;---
; Excerpted from "Programming Clojure, Second Edition",
; published by The Pragmatic Bookshelf.
; Copyrights apply to this code. It may not be used to create training material,
; courses, books, articles, and the like. Contact us if you are in doubt.
; We make no guarantees that this code is fit for any purpose.
; Visit http://www.pragmaticprogrammer.com/titles/shcloj2 for more book information.
;---
(ns programmingClojure.snake
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use examples.import-static))
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)
(def dirs { VK_LEFT [-1 0]
           VK_RIGHT [ 1 0]
           VK_UP [0 -1]
           VK_DOWN [ 0 1]})

(defn add-points [& pts]
  (vec (apply map + pts)))
(defn point-to-screen-rect [pt]
  (map #(* point-size %)
       [(pt 0) (pt 1) 1 1 ]))

(add-points [10 10] [-1 0])
(point-to-screen-rect [5 10])

(defn create-apple []
  {:location [(rand-int width) (rand-int height)]
   :color (Color. 210 50 90)
   :type :apple})

(defn create-snake []
  {:body (list [1 1])
   :dir [1 0]
   :type :snake
   :color (Color. 15 160 70)})

(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir)
                           (if grow body (butlast body)))))

(create-snake)
(move (create-snake))

(move (create-snake) :grow)

(defn win? [{body :body}]
  (>= (count body) win-length))
(win? {:body [[1 1u]]})

(win? {:body [[1 1] [1 2] [1 3] [1 4] [1 5]]})

(defn head-overlaps-body? [{[head & body] :body}]
  (contains? (set body) head))

(def lose? head-overlaps-body?)

(lose? {:body [[1 1] [1 2] [1 3]]})

(lose? {:body [[1 1] [1 2] [1 1]]})

(defn eats? [{[snake-head] :body} {apple :location}]
  (= snake-head apple))

(eats? {:body [[1 1] [1 2]]} {:location [2 2]})
(eats? {:body [[2 2] [1 2]]} {:location [2 2]})

(defn turn [snake newdir]
  (assoc snake :dir newdir))

(turn (create-snake) [0 -1])

(defn reset-game [snake apple]
  (dosync (ref-set apple (create-apple))
          (ref-set snake (create-snake)))
  nil)

(def test-snake (ref nil))
(def test-apple (ref nil))

(reset-game test-snake test-apple)
@test-snake
@test-apple

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))
(update-direction test-snake [0 -1])

(defn update-positions [snake apple]
  (dosync
   (if (eats? @snake @apple)
     (do (ref-set apple (create-apple))
         (alter snake move :grow))
     (alter snake move)))
  nil)

(reset-game test-snake test-apple)
(dosync (alter test-apple assoc :location [1 1]))
(update-positions test-snake test-apple)

(:body @test-snake)

(defn fill-point [g pt color]
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color)
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (:type object)))
(defmethod paint :apple [g {:keys [location color]}]
  (fill-point g location color))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defn game-panel [frame snake apple]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @apple))
    (actionPerformed [e]
      (update-positions snake apple)
      (when (lose? @snake)
        (reset-game snake apple)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e]
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [snake (ref (create-snake))
        apple (ref (create-apple))
        frame (JFrame. "Snake")
        panel (game-panel frame snake apple)
        timer (Timer. turn-millis panel)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)
    [snake, apple, timer]))
(game)
