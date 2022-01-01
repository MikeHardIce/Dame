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

(def ^:private player-color {:player1 [Color/white Color/black]
                             :player2 [Color/black Color/white]})

(defn draw-square
  ([^Board board x y color] (draw-square board x y color true))
  ([^Board board x y color fill]
   (c/draw-> (:canvas board)
             (c/rect (* x tile-size) (* y tile-size) color tile-size tile-size (not fill) 8)
             (c/text (* x tile-size) (- (* (inc y) tile-size)) (str "(" x "," y ")") Color/red 10))
  ;;  (c2d/with-canvas-> (:canvas board)
  ;;    (c2d/set-color color)
  ;;    (c2d/set-stroke 8)
  ;;    (c2d/rect (* x tile-size) (* y tile-size) tile-size tile-size (not fill))
  ;;    (c2d/set-color :red)
  ;;    (c2d/text (str "(" x "," y ")") (* x tile-size) (- (* (inc y) tile-size) 10)))
   ))

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
              (c/ellipse x0 y0 s0 s0 Color/white false 5)
              (c/ellipse x0 y0 s1 s1 Color/black false 5)
              (c/ellipse x0 y0 s2 s2 (->> player-color (player) (first)) false 5)
              (c/ellipse x0 y0 s3 s3 (->> player-color (player) (second)) false 5)
              (c/ellipse x0 y0 s4 s4 (->> player-color (player) (first)) true))
    ;; (c2d/with-canvas-> (:canvas board)
    ;;   (c2d/set-color :white)
    ;;   (c2d/ellipse x0 y0 s0 s0)
    ;;   (c2d/set-color :black)
    ;;   (c2d/ellipse x0 y0 s1 s1)
    ;;   (c2d/set-color (->> player-color (player) (first)))
    ;;   (c2d/ellipse x0 y0 s2 s2)
    ;;   (c2d/set-color (->> player-color (player) (second)))
    ;;   (c2d/ellipse x0 y0 s3 s3)
    ;;   (c2d/set-color (->> player-color (player) (first)))
    ;;   (c2d/ellipse x0 y0 s4 s4))
    ))

(defn draw-dame-sign
  "Draws an upsite down cross inside a D at the current tile"
  [^Board board x y player]
  (let [x0 (+ (* x tile-size) (* 0.5 tile-size))
        y0 (+ (* y tile-size) (* 0.5 tile-size))
        color (->> player-color (player) (second))]
    (c/draw-> (:canvas board)
              (c/text (inc x0) (+ y0 18) "D" color 48)
              (c/line x0 (- y0 15) x0 (+ y0 15) color 3)
              (c/line (- x0 10) (+ y0 5) (+ x0 10) (+ y0 5) color 3))
    ;; (c2d/with-canvas-> (:canvas board)
    ;;   (c2d/set-color (->> player-color (player) (second)))
    ;;   (c2d/set-font-attributes 48)
    ;;   (c2d/text "D" (inc x0) (+ y0 18) :center)
    ;;   (c2d/set-stroke 3)
    ;;   (c2d/line x0 (- y0 15) x0 (+ y0 15))
    ;;   (c2d/line (- x0 10) (+ y0 5) (+ x0 10) (+ y0 5))
    ;;   )
    ))

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
    (let [color (name (first (player player-color)))
          color (concat (list (s/upper-case (first color))) (rest color))
          color (apply str color)]
      (c/draw-> (:canvas board)
                (c/text 10 25 color 24 :bold))
      ;; (c2d/with-canvas-> (:canvas board)
      ;;   (c2d/set-color :red)
      ;;   (c2d/set-font-attributes 24 :bold)
      ;;   (c2d/text color 10 25))
      )))

(defn show-winner-banner
  ([^Board board player]
   (show-winner-banner board player 155 Color/white)
   (show-winner-banner board player 150 Color/red))
  ([^Board board player size color]
   (c/draw-> (:canvas board)
             (c/text tile-size (/ board-size 2) (s/upper-case (name player)) color size :italic)
             (c/text (* 3 tile-size) (/ board-size 1.5) "WON !!!" color size :italic))
  ;; (c2d/with-canvas-> (:canvas board)
  ;;   (c2d/set-color color)
  ;;   (c2d/set-font-attributes size :bold-italic)
  ;;   (c2d/text (s/upper-case (name player)) tile-size (/ board-size 2))
  ;;   (c2d/text "WON !!!" (* 3 tile-size) (/ board-size 1.5)))
   ))

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
  [_ _ widget]
  (when (not (:locked @(:current-board widget)))
    (let [window (:window @(:current-board widget))
          x (c2d/mouse-x window)
          y (c2d/mouse-y window)
          locked (:locked (swap! (:current-board widget) merge (click-board-at-tile @(:current-board widget) (get-tile x y))))]
      (swap! (:current-board widget) assoc :locked true)
      (swap! (:current-board widget) merge(click-board-at-tile @(:current-board widget) (get-tile x y) :after-tile-clicked))
      (when (not locked)
        (swap! (:current-board widget) assoc :locked nil))))
  widget)