(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic])
  (:gen-class))

(def game [[nil [:player2] nil [:player2] nil [:player2] nil [:player2]]
           [[:player2] nil [:player2] nil [:player2] nil [:player2] nil]
           [nil [:player2] nil [:player2] nil [:player2] nil [:player2]]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [[:player1] nil [:player1] nil [:player1] nil [:player1] nil]
           [nil [:player1] nil [:player1] nil [:player1] nil [:player1]]
           [[:player1] nil [:player1] nil [:player1] nil [:player1] nil]])

(defn -main
  ""
  []
  (let [board (board/create-board)]
    (board/draw-squares board)
    (board/draw-game board game)))

(defmethod board/game :tile-clicked 
  [_ coord]
  (println (str "tile: (" (first coord) "," (second coord) ")")))
