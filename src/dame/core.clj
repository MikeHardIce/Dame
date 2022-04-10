(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic]
            [strigui.core :as gui]
            [clojure.set :as s]
            [clojure.core.async :refer [go-loop timeout <!]])
  (:import [java.awt Color])
  (:gen-class))

(defonce game-start [[nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
                     [{:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil]
                     [nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]} nil {:player [:player2]}]
                     [nil nil nil nil nil nil nil nil]
                     [nil nil nil nil nil nil nil nil]
                     [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]
                     [nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]}]
                     [{:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil {:player [:player1]} nil]])

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
  ([game x y] (mark-stone game x y Color/green))
  ([game x y selection-color]
   (let [row (game y)
         element (nth row x)
         element (assoc element :selected true :selection-color selection-color)
         row (assoc row x element)]
     (assoc game y row))))

(defn get-marked-stone
  "gets the first marked stone with its coordinates via :coord"
  [game]
  (let [a (for [x (range (count game))
                y (range (count game))]
            (when-let [stone (nth (game y) x)]
              (when (:selected stone)
                (assoc stone :coord [x y]))))]
    (some identity a)))

(defn mark-moves
  [game moves]
  (loop [potential-moves moves
         game game]
    (if (seq potential-moves)
        (let [curr-x (first (first potential-moves))
              curr-y (second (first potential-moves))]
          (recur (rest potential-moves) (mark-stone game curr-x curr-y Color/yellow)))
      game)))

(defn start-game
  [widgets settings-player1 settings-player2]
  (let [board {:game game-start
              :players (list settings-player1 settings-player2)
              :restricted-moves []}]
   (-> widgets
       (assoc-in ["Dame" :board] board)
       (assoc-in ["Dame" :info-text] (-> board :players first first)))))

(defn create-play-mode-menu
  [widgets]
  (-> widgets
      (gui/add-button "btn-human" "vs Human" {:x 400 :y 300 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-human" :mouse-clicked (fn [wdgs _]
                                                     (-> wdgs
                                                         (gui/remove-widget-group "play-menu")
                                                         (start-game [:player1 :human] [:player2 :human]))))
      (gui/add-button "btn-computer-easy" "vs Computer (easy)" {:x 300 :y 450 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-computer-easy" :mouse-clicked (fn [wdgs _]
                                                             
                                                             (-> wdgs
                                                                 (gui/remove-widget-group "play-menu")
                                                                 (start-game [:player1 :human] [:player2 :easy]))))
      (gui/add-button "btn-easy-vs-easy" "Nah I just watch" {:x 300 :y 650 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-easy-vs-easy" :mouse-clicked (fn [wdgs _]
                                                             (-> wdgs
                                                                 (gui/remove-widget-group "play-menu")
                                                                 (start-game [:player1 :easy] [:player2 :easy]))))))

(defn create-start-btn
  [widgets]
    (-> widgets
        (gui/add-button "btn-start" "Start" {:x 400 :y 300 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "menu"})
        (gui/attach-event "btn-start" :mouse-clicked (fn [wdgs _]
                                                       (-> wdgs
                                                           (gui/remove-widget-group "menu")
                                                           (assoc-in ["Dame" :info-text] nil)
                                                           (create-play-mode-menu))))))

(defn create-quit-btn
  [widgets]
  (-> widgets
      (gui/add-button "btn-quit" "Quit" {:x 400 :y 500 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "menu"})
      (gui/attach-event "btn-quit" :mouse-clicked (fn [_ _]
                                                    (gui/close-window!)))))

(defn create-menu
  [widgets]
  (-> widgets
      create-start-btn
      create-quit-btn))

(defn -main
  ""
  []
  (gui/window! 400 400 1000 1000 "Dame")
  (let [board {:game game-start
              :players (list [:player1 :human] [:player2 :human])
              :restricted-moves []
              :current-player :player1}]
    (gui/swap-widgets! 
     (fn [wdgs]
       (-> wdgs
           (gui/add (board/create-board board))
           (assoc-in ["Dame" :locked] true)
           (create-menu))))))

(defn computer-easy-move
  [board]
  (let [computer-player (-> board :game :players first first)
        opponent (-> board :game :players second first)
        game (-> board :game)
        stones (for [x (range 8)
                     y (range 8)]
                 [x y (first (:player ((game y) x)))])
        stones (filter #(= (nth % 2 nil) computer-player) stones)
        stones (map #(into {:stone %} {:moves (logic/possible-moves game (first %) (second %))}) stones)
        stones (filter #(seq (:moves %)) stones)
        stones (map #(assoc % :opponent-stones-on-the-way (vec (for [move (:moves %)]
                                                                 (into {:destination move}
                                                                       {:opponent (logic/stones-on-the-way
                                                                                   game
                                                                                   (:stone %)
                                                                                   move opponent)})))) stones)
        good-moves (filter #(seq (->> % :opponent-stones-on-the-way :opponent)) stones)
        move (cond
               (seq good-moves) (let [m (rand-nth good-moves)]
                                  (vector (:stone m) (->> m :opponent-stones-on-the-way rand-nth :destination)))
               (seq stones) (let [m (rand-nth stones)]
                              (vector (:stone m) (->> m :moves rand-nth)))
               :else [])]
      (let [[x y] (second move)
            game (if (get-marked-stone game)
                   (logic/next-game game (first move) (second move))
                   (mark-stone game x y))
            board (assoc board :game game)]
        board)))

(defn init-computer-player 
  "Initializes a computer player, which is potentially its own thread"
  [player]
  (go-loop [winner (atom nil)]
    (<! (timeout 1000))
    (gui/swap-widgets! (fn [wdgs]
                         (let [dame-wdg (wdgs "Dame")
                               win (reset! winner (logic/get-winner (-> dame-wdg :board :game)))]
                           (if (= (-> dame-wdg :board :game :current-player) player)
                             (update-in wdgs ["Dame" :board] computer-easy-move))
                           wdgs)))
    (when (not @winner)
      (recur winner))))

(defmethod board/game :tile-clicked
 [_ board tile-clicked])
 

;; (defmethod board/game :tile-clicked
;;   [_ current-board coord]
;;   ;; either restricted moves is empty or the current clicked tile
;;   ;; belongs to a restricted move
;;   (when (or (not (seq @restrict-moves))
;;             (some #(= coord %) @restrict-moves))
;;     (let [x (first coord)
;;         y (second coord)
;;         moves (logic/possible-moves @game x y)
;;         tile ((@game y) x)
;;         player (first (:player tile))
;;         moves (if (= player (->> @current-player first first)) moves [])
;;         cnt-opponent-stones-before (count (logic/get-stones @game (->> @current-player second first)))]
;;     (if (and (seq tile) (:selected tile) (= (:selection-color tile) Color/yellow))
;;       ;; find the stone that was selected previously (jump from :green -> :yellow-green)
;;       (let [near-stones (for [xi (range 8)
;;                               yi (range 8)]
;;                           [xi yi (:selection-color ((@game yi) xi))])
;;             [[x0 y0]] (filter (fn [item]
;;                                 (some #(= % Color/green) item))
;;                               near-stones)]
;;         (swap! game logic/next-game [x0 y0] [x y])
;;         (swap! game unmark-all)
;;         (let [potential-restricted-moves (filter #(seq (logic/stones-on-the-way @game [x y] % (->> @current-player second first))) 
;;                                                  (logic/possible-moves @game x y))]
;;           (if (and (< (count (logic/get-stones @game (->> @current-player second first))) cnt-opponent-stones-before)
;;                    (seq potential-restricted-moves))
;;             (do ;; an opponent stone was removed and the player can remove another stone
;;               (swap! restrict-moves concat potential-restricted-moves)
;;               (swap! game mark-moves potential-restricted-moves)
;;               (swap! game mark-stone x y))
;;             (do ;; if nothing happened, then hand the turn over to the next player
;;               (swap! current-player reverse)
;;               (reset! restrict-moves [])))))
;;       (do ;; the player selected a tile that wasn't marked as :yellow-green 
;;         ;; (outside of :green -> :yellow-green move)
;;         (swap! game unmark-all)
;;         (swap! game mark-moves moves)
;;         (swap! game mark-stone x y)))))
;;     ;; redraw the game now with the potentially new state of the game
;;     (gui/update-skip-redraw! "Dame" :game @game)
;;     (board/draw-game current-board @game)
;;     ;(board/show-player-label current-board (->> @current-player first first))
;;     current-board
;;     (when-let [winner (logic/get-winner @game)]
;;       (board/show-winner-banner current-board winner)
;;       (create-menu)
;;       (assoc current-board :locked true)))

;; (defmethod board/game :after-tile-clicked
;;   [_ current-board coord]
;;   (if (and (not= (->> @current-player first second) :human)
;;            (not (logic/get-winner @game)))
;;     (computer-easy-move current-board)
;;     current-board))