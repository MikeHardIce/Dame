(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic]
            [strigui.core :as gui])
  (:gen-class))

(defonce game-start [[nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
                     [{:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil]
                     [nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
                     [nil nil nil nil nil nil nil nil]
                     [nil nil nil nil nil nil nil nil]
                     [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]
                     [nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]}]
                     [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]])

(def game (atom game-start))

(def current-player (atom '([:player1 :human] [:player2 :human])))

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

(defn start-game
  [player1-setting player2-setting]
  (let [board (gui/find-by-name "Dame")]
    (reset! game game-start)
    (swap! (:current-board board) assoc :locked nil)
    (gui/remove! "btn-human")
    (gui/remove! "btn-computer-easy")
    (reset! current-player (list player1-setting player2-setting))
    (gui/update! "Dame" :info-text (->> @current-player first first))))

(defn create-play-mode-menu
  []
  (gui/button "btn-human" "vs Human" {:x 400 :y 300 :z 3 :min-width 250 :color [:white :black] :font-size 28})
  (gui/update! "btn-human" [:events :mouse-clicked] (fn [_]
                                                      (start-game [:player1 :human] [:player2 :human])))
  (gui/button "btn-computer-easy" "vs Computer (easy)" {:x 400 :y 450 :z 3 :min-width 250 :color [:white :black] :font-size 28})
  (gui/update! "btn-computer-easy" [:events :mouse-clicked] (fn [_]
                                                      (start-game [:player1 :human] [:player2 :easy]))))

(defn create-start-btn
  []
  (gui/button "btn-start" "Start" {:x 400 :y 300 :z 3 :min-width 250 :color [:white :black] :font-size 28})
  (gui/update! "btn-start" [:events :mouse-clicked] (fn [_]
                                                      (gui/remove! "btn-start")
                                                      (gui/remove! "btn-quit")
                                                      (gui/update! "Dame" :info-text nil)
                                                      (create-play-mode-menu))))

(defn create-quit-btn
  []
  (gui/button "btn-quit" "Quit" {:x 400 :y 500 :z 3 :min-width 250 :color [:white :black] :font-size 28})
  (gui/update! "btn-quit" [:events :mouse-clicked] (fn [_]
                                                     (gui/close-window))))

(defn create-menu
  []
  (create-start-btn)
  (create-quit-btn))

(defn -main
  ""
  []
  (let [window (gui/window! 1000 1000 "Dame")
        game-board (board/create-board (:canvas window) (:window window) @game)]
    (swap! (:current-board game-board) assoc :locked true)
    (gui/create game-board)
    (create-menu)))

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
        moves (if (= player (->> @current-player first first)) moves [])
        cnt-opponent-stones-before (count (logic/get-stones @game (->> @current-player second first)))]
    (if (and (seq tile) (:selected tile) (= (:selection-color tile) :yellow-green))
      ;; find the stone that was selected previously (jump from :green -> :yellow-green)
      (let [near-stones (for [xi (range 8)
                              yi (range 8)]
                          [xi yi (:selection-color ((@game yi) xi))])
            [[x0 y0]] (filter (fn [item]
                                (some #(= % :green) item))
                              near-stones)]
        (swap! game logic/next-game [x0 y0] [x y])
        (swap! game unmark-all)
        (let [potential-restricted-moves (filter #(seq (logic/stones-on-the-way @game [x y] % (->> @current-player second first))) 
                                                 (logic/possible-moves @game x y))]
          (if (and (< (count (logic/get-stones @game (->> @current-player second first))) cnt-opponent-stones-before)
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
    (board/show-player-label current-board (->> @current-player first first))
    current-board
    (when-let [winner (logic/get-winner @game)]
      (board/show-winner-banner current-board winner)
      (create-menu)
      (assoc current-board :locked true)))

(defmethod board/game :after-tile-clicked
  [_ current-board coord]
  (when (not= (->> @current-player first second) :human)
    (let [stones (for [x (range 8)
                       y (range 8)]
                   [x y (first (:player ((@game y) x)))])
          bla (println stones)
          stones (filter #(= (nth % 2 nil) (->> @current-player first first)) stones)
          bla (println stones)
          stones (map #(into {:stone %} {:moves (logic/possible-moves @game (first %) (second %))}) stones)
          stones (filter #(seq (:moves %)) stones)
          bla (println stones)
          stones (map #(assoc % :opponent-stones-on-the-way (vec (for [move (:moves %)]
                                                                   (into {:destination move}
                                                                         {:opponent (logic/stones-on-the-way
                                                                                     @game
                                                                                     (:stone %)
                                                                                     move (->> @current-player
                                                                                               second
                                                                                               first))})))) stones)
          bla (println stones)
          good-moves (filter #(seq (->> % :opponent-stones-on-the-way :opponent)) stones)
          move (if (seq good-moves)
                 (let [m (rand-nth good-moves)]
                   (vector (:stone m) (->> m :opponent-stones-on-the-way rand-nth :destination)))
                 (let [m (rand-nth stones)]
                   (vector (:stone m) (->> m :moves rand-nth))))
          current-board (merge current-board (board/click-board-at-tile current-board (first move)))
          current-board (do
                          ;;delay
                          (merge current-board (board/click-board-at-tile current-board (second move))))]
      current-board))
current-board)