(ns dame.game-board
  (:require [capra.core :as c]
            [clojure.string :as s]
            [strigui.widget :as wdg])
  (:import [java.awt Color]))

(def ^:private board-size 1000)
(def ^:private tile-size 125)

(def ^:private player-color {:player1 [Color/white Color/black "White"]
                             :player2 [Color/black Color/white "Black"]})

(defn draw-square
  ([canvas x y color] (draw-square canvas x y color true))
  ([canvas x y color fill]
   (c/draw-> canvas
             (c/rect (* x tile-size) (* y tile-size) tile-size tile-size color fill 8)
             ;;(c/text (* x tile-size) (* y tile-size) (str "(" x "," (dec y) ")") Color/red 14)
             )))

(defn draw-stone
  [canvas x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        s0 (* tile-size 0.82)
        s1 (* tile-size 0.8)
        s2 (* tile-size 0.75)
        s3 (* tile-size 0.55)
        s4 (* tile-size 0.45)]
    (c/draw-> canvas
              (c/ellipse x0 y0 s0 s0 Color/white false 10)
              (c/ellipse x0 y0 s1 s1 Color/black false 10)
              (c/ellipse x0 y0 s2 s2 (->> player-color player first) false 10)
              (c/ellipse x0 y0 s3 s3 (->> player-color player second) false 10)
              (c/ellipse x0 y0 s4 s4 (->> player-color player first) true))))

(defn draw-dame-sign
  "Draws an upsite down cross inside a D at the current tile"
  [canvas x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        color (->> player-color player second)]
    (c/draw-> canvas
              (c/text (- x0 17) (+ y0 18) "D" color 50)
              (c/line (+ x0 3) (- y0 15) (+ x0 3) (+ y0 15) color 3)
              (c/line (- x0 7) (+ y0 3) (+ x0 13) (+ y0 3) color 3))))

(defn draw-game
  "game is a 8 element vector with each element being a 8 element vector"
  [canvas game]
  (doseq [x (range 8)
          y (range 8)]
    (let [stone ((game y) x)
          color [Color/white Color/black]
          color-indicator (mod (+ x y) 2)]
      (if (and (:selected stone) (:selection-color stone))
        (draw-square canvas x y (:selection-color stone) false)
        (draw-square canvas x y (nth color color-indicator)))
      (let [stones (seq (:player stone))
            player (nth stones 0)]
        (when player
          (draw-stone canvas x y player)
          (when (> (count stones) 1)
            (draw-dame-sign canvas x y player)))))))

(defn show-player-label
  [canvas player]
  (when player
    (let [color (name (nth (player player-color) 2))
          color (apply str color)]
      (c/draw-> canvas
                (c/rect 0 0 tile-size tile-size Color/white true)
                (c/text 10 25 color Color/red 24 :bold)))))

(defn show-banner
  ([canvas player]
   (show-banner canvas player 155 Color/white)
   (show-banner canvas player 150 Color/red))
  ([canvas player size color]
   (c/draw-> canvas
             (c/text tile-size (/ board-size 2) (s/upper-case player) color size :italic)
             (c/text (* 3 tile-size) (/ board-size 1.5) "WON !!!" color size :italic))))

(defn get-tile
  "Returns the tile that contains the given x and y coordinates"
  [x-coord y-coord]
  (let [x (Math/floor (/ x-coord tile-size))
        y (Math/floor (/ y-coord tile-size))]
    [(int x) (int y)]))

(defrecord Game-Board [name board locked props big-text]
  wdg/Widget
  (coord [this context] [0 0 board-size board-size])
  (defaults [this] (assoc-in this [:props :can-hide?] false))
  (before-drawing [this] this)
  (draw [this context]
          (draw-game context (-> this :board :game))
          (when-let [banner (:big-text this)]
            (show-banner context banner))
          (show-player-label context (-> this :board :players first first))
        this)
  (after-drawing [this] this))

(defmulti game (fn [type ^Game-Board board data] type))

(defmethod game :default [type board data] board)

(defn create-board
  [name game]
    (->Game-Board name game nil {:x 0 :y 0 :z -5} nil))

(defmethod wdg/widget-event [dame.game_board.Game-Board :mouse-clicked]
  [widgets {:keys [widget x y]}]
  (if (not (:locked widget))
    (let [widget (game :tile-clicked widget (get-tile x y))]
      (assoc widgets (:name widget) widget))
    widgets))