(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic]
            [strigui.core :as gui]
            [clojure.set :as s])
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
  ([game x y] (mark-stone game x y Color/green))
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
          (recur (rest potential-moves) (mark-stone game curr-x curr-y Color/yellow)))
      game)))

(defn start-game
  [widgets settings-player1 settings-player2]
  (let [game {:board game-start
              :players (list settings-player1 settings-player2)
              :restricted-moves []}]
   (-> widgets
       (assoc-in ["Dame" :game] game)
       (assoc-in ["Dame" :info-text] (-> current-player first first)))))

(defn dissoc-widgets-by-group
  [widgets group-name]
  (let [widget-keys (->> (gui/find-widgets-by-group-name widgets group-name)
                         (map :name))
        keys-to-keep (s/difference (set (map :name widgets)) (set widget-keys))]
    (select-keys widgets keys-to-keep)))

(defn create-play-mode-menu
  [widgets]
  (-> widgets
      (gui/add-button "btn-human" "vs Human" {:x 400 :y 300 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-human" :mouse-clicked (fn [wdgs _]
                                                     (-> wdgs
                                                         (dissoc-widgets-by-group "play-menu")
                                                         (start-game [:player1 :human] [:player2 :human]))))
      (gui/add-button "btn-computer-easy" "vs Computer (easy)" {:x 300 :y 450 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-computer-easy" :mouse-clicked (fn [wdgs _]
                                                             (-> wdgs
                                                                 (dissoc-widgets-by-group "play-menu")
                                                                 (start-game [:player1 :human] [:player2 :easy]))))
      (gui/add-button "btn-easy-vs-easy" "Nah I just watch" {:x 300 :y 650 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-easy-vs-easy" :mouse-clicked (fn [wdgs _]
                                                             (-> wdgs
                                                                 (dissoc-widgets-by-group "play-menu")
                                                                 (start-game [:player1 :easy] [:player2 :easy]))))))

(defn create-start-btn
  [widgets]
    (-> widgets
        (gui/add-button "btn-start" "Start" {:x 400 :y 300 :z 3 :width 250 :color [Color/white Color/black] :font-size 28 :group "menu"})
        (gui/attach-event "btn-start" :mouse-clicked (fn [wdgs _]
                                                       (-> wdgs
                                                           (dissoc-widgets-by-group "menu")
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
  (let [game {:board game-start
              :players (list [:player1 :human] [:player2 :human])
              :restricted-moves []}]
    (gui/swap-widgets! 
     (fn [wdgs]
       (-> wdgs
           (gui/add (board/create-board game))
           (assoc-in ["Dame" :locked] true)
           (create-menu))))))

(defn computer-easy-move 
  [current-board]
  (let [computer-player (->> @current-player first first)
        stones (for [x (range 8)
                     y (range 8)]
                 [x y (first (:player ((@game y) x)))])
        stones (filter #(= (nth % 2 nil) (->> @current-player first first)) stones)
        stones (map #(into {:stone %} {:moves (logic/possible-moves @game (first %) (second %))}) stones)
        stones (filter #(seq (:moves %)) stones)
        stones (map #(assoc % :opponent-stones-on-the-way (vec (for [move (:moves %)]
                                                                 (into {:destination move}
                                                                       {:opponent (logic/stones-on-the-way
                                                                                   @game
                                                                                   (:stone %)
                                                                                   move (->> @current-player
                                                                                             second
                                                                                             first))})))) stones)
        good-moves (filter #(seq (->> % :opponent-stones-on-the-way :opponent)) stones)
        move (cond
               (seq good-moves) (let [m (rand-nth good-moves)]
                                  (vector (:stone m) (->> m :opponent-stones-on-the-way rand-nth :destination)))
               (seq stones) (let [m (rand-nth stones)]
                              (vector (:stone m) (->> m :moves rand-nth)))
               :else [])]
    (loop [moves (vector (first move) (second move))
           board current-board]
      (let [new-board (merge board (board/click-board-at-tile board (first moves)))
            new-moves (rest moves)
            new-moves (if (and (= (->> @current-player first first) computer-player)
                               (not (seq new-moves)))
                        @restrict-moves
                        new-moves)]
        (if (seq new-moves)
          (do
            (Thread/sleep 500)
            (recur new-moves new-board))
          new-board)))))

(defmethod board/game :tile-clicked
 [args])
 

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