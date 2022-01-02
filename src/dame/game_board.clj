(ns dame.game-board
  (:require [capra.core :as c]
            [clojure.string :as s]
            [strigui.widget :as wdg])
  (:import [java.awt Color]))

(defrecord Board [canvas window locked])

(defmulti game (fn [type ^Board board data] type))

(defmethod game :default [])

(def ^:private board-size 1000)
(def ^:private tile-size 125)

(def ^:private player-color {:player1 [Color/white Color/black "White"]
                             :player2 [Color/black Color/white "Black"]})

(defn draw-square
  ([^Board board x y color] (draw-square board x y color true))
  ([^Board board x y color fill]
   (c/draw-> (:canvas board)
             (c/rect (* x tile-size) (* y tile-size) tile-size tile-size color fill 8)
             (c/text (* x tile-size) (- (* (inc y) tile-size)) (str "(" x "," y ")") Color/red 10))))

(defn draw-stone
  [^Board board x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        s0 (* tile-size 0.82)
        s1 (* tile-size 0.8)
        s2 (* tile-size 0.75)
        s3 (* tile-size 0.55)
        s4 (* tile-size 0.45)]
    (c/draw-> (:canvas board)
              (c/ellipse x0 y0 s0 s0 Color/white false 10)
              (c/ellipse x0 y0 s1 s1 Color/black false 10)
              (c/ellipse x0 y0 s2 s2 (->> player-color (player) (first)) false 10)
              (c/ellipse x0 y0 s3 s3 (->> player-color (player) (second)) false 10)
              (c/ellipse x0 y0 s4 s4 (->> player-color (player) (first)) true))))

(defn draw-dame-sign
  "Draws an upsite down cross inside a D at the current tile"
  [^Board board x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        color (->> player-color (player) (second))]
    (c/draw-> (:canvas board)
              (c/text (- x0 17) (+ y0 18) "D" color 50)
              (c/line (+ x0 3) (- y0 15) (+ x0 3) (+ y0 15) color 3)
              (c/line (- x0 7) (+ y0 3) (+ x0 13) (+ y0 3) color 3))))

(defn draw-game
  "game is a 8 element vector with each element being a 8 element vector"
  [^Board board game]
  (doseq [x (range 8)
          y (range 8)]
    (let [stone ((game y) x)
          color [Color/white Color/black]
          color-indicator (mod (+ x y) 2)]
      (if (and (:selected stone) (:selection-color stone))
        (draw-square board x y (:selection-color stone) false)
        (draw-square board x y (nth color color-indicator)))
      (let [stones (seq (:player stone))
            player (nth stones 0)]
        (when player
          (draw-stone board x y player)
          (when (> (count stones) 1)
            (draw-dame-sign board x y player)))))))

(defn show-player-label
  [^Board board player]
  (when player
    (let [color (name (nth (player player-color) 2))
          color (apply str color)]
      (c/draw-> (:canvas board)
                (c/text 10 25 color Color/red 24 :bold)))))

(defn show-winner-banner
  ([^Board board player]
   (show-winner-banner board player 155 Color/white)
   (show-winner-banner board player 150 Color/red))
  ([^Board board player size color]
   (c/draw-> (:canvas board)
             (c/text tile-size (/ board-size 2) (s/upper-case (name player)) color size :italic)
             (c/text (* 3 tile-size) (/ board-size 1.5) "WON !!!" color size :italic))))

(defn select-stone
  [^Board board x y]
  (draw-square board x y Color/green false))

(defn get-tile
  "Returns the tile that contains the given x and y coordinates"
  [x-coord y-coord]
  (let [x (Math/floor (/ x-coord tile-size))
        y (Math/floor (/ y-coord tile-size))]
    [(int x) (int y)]))

(defrecord Game-Board [name game current-board args info-text]
  wdg/Widget
  (coord [this canvas] [0 0 board-size board-size])
  (defaults [this] (assoc-in this [:args :skip-redrawing] {:on-unselect true :on-click true :on-hover true}))
  (draw [this canvas]
        (draw-game @(:current-board this) (:game this))
        (show-player-label @(:current-board this) (:info-text this))
        this))

(defn create-board
  [canvas window game]
  (let [current-board (atom (->Board canvas window nil))]
    (->Game-Board "Dame" game current-board {:x 0 :y 0 :z -5} nil)))

(defn click-board-at-tile
  ([board tile] (click-board-at-tile board tile :tile-clicked))
  ([board tile event]
   (game event board tile)))

(defmethod wdg/widget-event [dame.game_board.Game-Board :mouse-clicked]
  [_ _ widget x y]
  (when (not (:locked @(:current-board widget)))
    (let [locked (:locked (swap! (:current-board widget) merge (click-board-at-tile @(:current-board widget) (get-tile x y))))]
      (swap! (:current-board widget) assoc :locked true)
      (swap! (:current-board widget) merge(click-board-at-tile @(:current-board widget) (get-tile x y) :after-tile-clicked))
      (when (not locked)
        (swap! (:current-board widget) assoc :locked nil))))
  widget)