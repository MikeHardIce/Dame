(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic])
  (:gen-class))

(def game (atom [[nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
           [{:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil]
           [nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]
           [nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]}]
           [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]]))

(defn -main
  ""
  []
  (let [board (board/create-board)]
    (board/draw-squares board)
    (board/draw-game board @game)))

(defmethod board/game :tile-clicked
  [_ current-board coord]
  (let [x (first coord)
        y (second coord)]
    (println (str "tile: (" x "," y ")"))
    (board/draw-squares current-board)
    (board/draw-game current-board @game)
    (loop [potential-moves (conj (logic/possible-moves @game x y) [x y])]
      (when (seq potential-moves)
        (let [curr-x (first (first potential-moves))
              curr-y (second (first potential-moves))]
          ;; TODO: Actually dont expose this here, each tile can be a map, with a key :selected
          ;;           and just let the game board draw the selections 
          (board/draw-square current-board curr-x curr-y :green false))
        (recur (rest potential-moves))))))
