(ns dame.core
  (:require [dame.game-board :as board])
  (:gen-class))

(def game [[:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [:player1 :player1 :player1 :player1 :player1 :player1 :player1 :player1]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]
           [:player2 :player2 :player2 :player2 :player2 :player2 :player2 :player2]])

(defn main
  ""
  []
  (let [board (board/create-board)]
    (board/draw-squares board)
    (board/draw-game board game)
    (board/select-stone board 2 3)))
