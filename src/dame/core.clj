(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic]
            [strigui.core :as gui])
  (:gen-class))

(def game (atom [[nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
           [{:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil]
           [nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
           [nil nil nil nil nil nil nil nil]
           [nil nil nil nil nil nil nil nil]
           [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]
           [nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]}]
           [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]]))

;; (def game (atom [[nil {:player [:player2 :player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
;;                  [{:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil]
;;                  [nil {:player [:player2]} nil {:player [:player2 :player2]} nil {:player [:player2]} nil {:player [:player2]}]
;;                  [{:player [:player1]} nil nil nil nil nil {:player [:player1]} nil]
;;                  [nil nil nil nil nil nil nil nil]
;;                  [nil nil nil nil nil nil nil nil]
;;                  [nil nil nil nil nil nil nil nil]
;;                  [nil nil nil nil  nil nil nil nil]]))

(def current-player (atom '(:player1 :player2)))

(def restrict-moves (atom []))

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

(defn create-start-btn
  []
  (gui/button "btn-start" "Start" {:x 400 :y 300 :min-width 250 :color [:white :black]})
  (gui/update! "btn-start" [:events :mouse-clicked] (fn [_]
                                                      (let [board (gui/find-by-name "Dame")]
                                                        (swap! (:current-board board) assoc :locked nil)
                                                        (gui/remove! "btn-start")
                                                        (gui/remove! "btn-quit")
                                                        (gui/update! "Dame" :info-text (first @current-player))))))

(defn create-quit-btn
  []
  (gui/button "btn-quit" "Quit" {:x 400 :y 500 :min-width 250 :color [:white :black]}))

(defn -main
  ""
  []
  (let [window (gui/window! 1000 1000 "Dame")
        game-board (board/create-board (:canvas window) (:window window) @game)]
    (swap! (:current-board game-board) assoc :locked true)
    (gui/create game-board)
    (create-start-btn)
    (create-quit-btn)))

(defmethod board/game :tile-clicked
  [_ current-board coord]
  ;; either restricted moves is empty or the current clicked tile
  ;; belongs to a restricted move
  (when (or (not (seq @restrict-moves))
            (some #(= coord %) @restrict-moves))
    (let [x (first coord)
        y (second coord)
        moves (logic/possible-moves @game x y)
        tile ((@game y) x)
        player (first (:player tile))
        moves (if (= player (first @current-player)) moves [])
        cnt-opponent-stones-before (count (logic/get-stones @game (second @current-player)))]
    (if (and (seq tile) (:selected tile) (= (:selection-color tile) :yellow-green))
      ;; find the stone that was selected previously (jump from :green -> :yellow-green)
      (let [near-stones (for [xi (range 8)
                              yi (range 8)]
                          [xi yi (:selection-color ((@game yi) xi))])
            [[x0 y0]] (filter (fn [item]
                                (some #(= % :green) item))
                              near-stones)]
        (println " [" x0 " " y0 "] [" x " " y "] ")
        (swap! game logic/next-game [x0 y0] [x y])
        (swap! game unmark-all)
        (let [potential-restricted-moves (filter #(seq (logic/stones-on-the-way @game [x y] % (second @current-player))) 
                                                 (logic/possible-moves @game x y))]
          (if (and (< (count (logic/get-stones @game (second @current-player))) cnt-opponent-stones-before)
                   (seq potential-restricted-moves))
            (do ;; an opponent stone was removed and the player can remove another stone
              (swap! restrict-moves concat potential-restricted-moves)
              (swap! game mark-moves potential-restricted-moves)
              (swap! game mark-stone x y))
            (do ;; if nothing happened, then hand the turn over to the next player
              (swap! current-player reverse)
              (reset! restrict-moves [])))))
      (do ;; the player selected a tile that wasn't marked as :yellow-green 
        ;; (outside of :green -> :yellow-green move)
        (swap! game unmark-all)
        (swap! game mark-moves moves)
        (swap! game mark-stone x y)))))
    ;; redraw the game now with the potentially new state of the game
    (board/draw-game current-board @game)
    (board/show-player-label current-board (first @current-player))
    current-board
    (when-let [winner (logic/get-winner @game)]
      (board/show-winner-banner current-board winner)
      (assoc current-board :locked true)))