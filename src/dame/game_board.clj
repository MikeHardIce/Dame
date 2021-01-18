(ns dame.game-board
  (:require [clojure2d.core :as c2d]))

(defrecord Board [canvas window])

(def ^:private board-size 1000)
(def ^:private tile-size 125)

(def ^:private player-color {:player1 [:white :black]
                             :player2 [:black :white]})

(def game [[:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]])

(defn draw-squares
  [^Board board]
  (loop [x 0
         y 0
         colors [:white :black]]
    (when (and (< (+ x y) 15))
      (c2d/with-canvas-> (:canvas board)
        (c2d/set-color (first colors))
        (c2d/rect (* x tile-size) (* y tile-size) (* (inc x) tile-size) (* (inc y) tile-size)) )
      (recur (if (> x 7) 0 (inc x))
             (if (> x 7) (inc y) y)
             (reverse colors)))))

(defn draw-stone
  [^Board board x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        s0 (* tile-size 0.82)
        s1 (* tile-size 0.8)
        s2 (* tile-size 0.75)
        s3 (* tile-size 0.55)
        s4 (* tile-size 0.45)]
    (c2d/with-canvas-> (:canvas board)
      (c2d/set-color :white)
      (c2d/ellipse x0 y0 s0 s0)
      (c2d/set-color :black)
      (c2d/ellipse x0 y0 s1 s1)
      (c2d/set-color (->> player-color (player) (first)))
      (c2d/ellipse x0 y0 s2 s2)
      (c2d/set-color (->> player-color (player) (second)))
      (c2d/ellipse x0 y0 s3 s3)
      (c2d/set-color (->> player-color (player) (first)))
      (c2d/ellipse x0 y0 s4 s4))))

(defn draw-game
  "game is a 8 element vector with each element being a 8 element vector"
  [^Board board game]
  (doseq [x (range 0 8)
          y (range 0 8)]
    (when-let [player ((game y) x)]
      (draw-stone board x y player))))

(defn create-board
  []
  (let [canvas (c2d/canvas board-size board-size)]
    (Board. canvas (c2d/show-window canvas "Dame"))))