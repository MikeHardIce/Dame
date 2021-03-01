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

(defn unmark-all 
  [game]
  (let [new-game (for [y (range (count game))
                       x (range (count (game y)))]
                   (let [stone (nth (game y) x)]
                     (dissoc stone :selected :selection-color)))
        new-game (partition (count game) new-game)
        new-game (map vec new-game)]
    (vec new-game)))

(defn mark-stone 
  ([game x y] (mark-stone game x y :green))
  ([game x y selection-color]
  (let [row (game y)
        element (nth row x)
        element (assoc element :selected true :selection-color selection-color)
        row (assoc row x element)]
    (assoc game y row))))

(defn mark-moves
  [game moves]
  (loop [potential-moves moves
         game game]
    (if (seq potential-moves)
        (let [curr-x (first (first potential-moves))
              curr-y (second (first potential-moves))]
          (recur (rest potential-moves) (mark-stone game curr-x curr-y :yellow-green)))
      game)))

(defn -main
  ""
  []
  (let [board (board/create-board)]
    (board/draw-game board @game)))

(defmethod board/game :tile-clicked
  [_ current-board coord]
  (let [x (first coord)
        y (second coord)
        moves (logic/possible-moves @game x y)
        tile ((@game y) x)]
    (println (str "tile: (" x "," y ")"))
    (if (and (seq tile) (:selected tile) (= (:selection-color tile) :yellow-green))
      (let [y-range (cond 
                      (and (> y 0) (< y 7)) [(dec y) (inc y)]
                      (= y 0) [1]
                      :else [6])
            near-stones (for [xi (range 8)
                              yi y-range]
                          [xi yi (:selection-color ((@game yi) xi))])
            [[x0 y0]] (filter (fn [item]
                                (some #(= % :green) item)) 
                              near-stones)]
        (println (str " [" x0 " " y0 "] [" x " " y "] "))
        (swap! game logic/next-game [x0 y0] [x y])
        (swap! game unmark-all))
      (do
        (swap! game unmark-all)
        (swap! game mark-moves moves)
        (swap! game mark-stone x y)))
    (board/draw-game current-board @game)))