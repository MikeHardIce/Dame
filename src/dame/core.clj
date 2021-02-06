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
  [_ current-board coord]
  (let [x (first coord)
        y (second coord)]
    (println (str "tile: (" x "," y ")"))
    (board/draw-squares current-board)
    (board/draw-game current-board game)
    (loop [potential-moves (conj (logic/possible-moves game x y) [x y])]
      (when (seq potential-moves)
        (let [curr-x (first (first potential-moves))
              curr-y (second (first potential-moves))]
          (board/draw-square current-board curr-x curr-y :green false))
        (recur (rest potential-moves))))))
