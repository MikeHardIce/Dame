(ns dame.core
  (:require [dame.game-board :as board]
            [dame.game-logic :as logic]
            [strigui.core :as gui]
            [clojure.set :as s]
            [clojure.core.async :refer [go-loop timeout <!]])
  (:import [java.awt Color])
  (:gen-class))

(defonce color-for-button [(Color. 220 220 150) (Color/black)])

(defonce zig-zag [[nil nil nil nil nil {:player [:player2]} nil nil]
                  [nil nil nil nil nil nil nil nil]
                  [nil nil nil {:player [:player2]} nil nil nil nil]
                  [nil nil nil nil nil nil nil nil]
                  [nil {:player [:player2]} nil nil nil nil nil nil]
                  [{:player [:player1]} nil nil nil nil nil nil nil]
                  [nil nil nil nil nil nil nil nil]
                  [nil nil nil nil nil nil nil nil]])

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
  ([game] (get-marked-stone game Color/green))
  ([game color]
  (let [a (for [x (range (count game))
                y (range (count game))]
            (when-let [stone (nth (game y) x)]
              (when (and (:selected stone) (= (:selection-color stone) color))
                (assoc stone :coord [x y]))))]
    (filterv identity a))))

(defn mark-moves
  [game moves]
  (loop [potential-moves moves
         game game]
    (if (seq potential-moves)
        (let [curr-x (first (first potential-moves))
              curr-y (second (first potential-moves))]
          (recur (rest potential-moves) (mark-stone game curr-x curr-y Color/yellow)))
      game)))

(defn get-moves-for
  "returns a map {:stone [x y] :moves [[x0 y0] [x1 y1] ...]}"
  [game [x y]]
  (into {:stone [x y]} {:moves (logic/possible-moves game x y)}))

(defn get-all-stones-with-moves
  "returns a sequence of maps of the form {:stone [x y] :moves [[x0 y0] [x1 y1] ...]}
   and filters the sequence to only contain the maps containing moves"
  [game current-player]
  (let [stones (for [x (range (count game))
                     y (range (count game))]
                 [x y (first (:player ((game y) x)))])
        stones (filter #(= (nth % 2 nil) current-player) stones)
        stones (map (partial get-moves-for game) stones)]
    (filter #(seq (:moves %)) stones)))

(defn moves-with-opponent-on-way
  "requires a map of the form {:stone [x y] :moves [[x0 y0] [x1 y1] ...]}
   returns a vector with moves that have an opponent on the way"
  [game opponent stone-with-moves]
  (filterv #(seq (logic/stones-on-the-way game (:stone stone-with-moves) % opponent)) (:moves stone-with-moves)))

(defn computer-easy-move
  [board]
  (let [computer-player (-> board :players first first)
        opponent (-> board :players second first)
        game (-> board :game)
        coord-marked (-> game get-marked-stone first :coord)]
    (if (seq coord-marked)
      (let [stone-with-moves (get-moves-for game coord-marked)
            moves-with-opponents (moves-with-opponent-on-way game opponent stone-with-moves)]
        (if (seq moves-with-opponents)
          (let [move (rand-nth moves-with-opponents)
                board (update board :game logic/next-game coord-marked move)
                game (:game board)
                can-jump-more (get-moves-for game move)
                can-jump-more (moves-with-opponent-on-way game opponent can-jump-more)]
            (if (seq can-jump-more)
              (update board :game mark-stone (first move) (second move))
              (-> board
                  (update :game unmark-all)
                  (update :players reverse))))
          (let [move (rand-nth (:moves stone-with-moves))]
            (-> board
                (update :game logic/next-game coord-marked move)
                (update :game unmark-all)
                (update :players reverse)))))
      (let [stones (get-all-stones-with-moves game computer-player) ; fresh turn
            stone-w-opponents (filter (partial moves-with-opponent-on-way game opponent) stones)
            stones (if (seq stone-w-opponents) stone-w-opponents stones)
            stone (rand-nth stones)
            [x y] (:stone stone)
            moves (:moves stone)]
        (if (seq stone)
          (-> board
              (update :game mark-stone x y)
              (update :game mark-moves moves))
          (update board :players reverse))))))

(defn init-computer-player 
  "Initializes a computer player, which is potentially its own thread"
  [player]
  (go-loop [winner (atom nil)]
    (<! (timeout 2000))
    (gui/swap-widgets! (fn [wdgs]
                         (let [dame-wdg (wdgs "Dame")
                               win (reset! winner (logic/get-winner (-> dame-wdg :board :game)))
                               wdgs (if (and (= (-> dame-wdg :board :players first first) player)
                                             (not win))
                                      (update-in wdgs ["Dame" :board] computer-easy-move)
                                      (if win
                                        (let [fn-exit (-> dame-wdg :board :fn-exit)]
                                          (-> wdgs
                                              (assoc-in ["Dame" :big-text] win)
                                              fn-exit))
                                        wdgs))
                               player-type (-> (wdgs "Dame") :board :players first second)]
                               (assoc-in wdgs ["Dame" :locked] (not= :human player-type)))))
    (when (not @winner)
      (recur winner))))

(defn human-player-move
  [board tile-clicked]
  (let [player (-> board :players first first)
        opponent (-> board :players second first)
        game (-> board :game)
        coords-marked (mapv :coord (-> game (get-marked-stone Color/yellow)))]
    (if (some #(= % tile-clicked) coords-marked)
      (let [green-tile (->> (get-marked-stone game Color/green) first :coord)
            [x y] tile-clicked
            opponent-on-current-way (logic/stones-on-the-way game green-tile tile-clicked opponent)
            board (update board :game logic/next-game green-tile tile-clicked)
            game (:game board)
            further-moves (->> tile-clicked
                               (get-moves-for game)
                               (moves-with-opponent-on-way game opponent))
            board (if (and (seq further-moves) (seq opponent-on-current-way))
                    (loop [board (update board :game mark-stone x y)
                           moves further-moves]
                      (if (seq moves)
                        (recur (update board :game mark-stone (-> moves first first) (-> moves first second) Color/yellow)
                               (rest moves))
                        board))
                    (-> board
                        (update :game unmark-all)
                        (update :players reverse)))]
        board)
      (let [[x y] tile-clicked
            board (-> board
                      (update :game unmark-all)
                      (update :game mark-stone x y))
            tile-owned-by (first (:player ((game y) x)))]
        (if (= tile-owned-by player)
          (let [further-moves (:moves (get-moves-for game tile-clicked))
                board (if (seq further-moves)
                        (loop [board (update board :game mark-stone x y)
                               moves further-moves]
                          (if (seq moves)
                            (recur (update board :game mark-stone (-> moves first first) (-> moves first second) Color/yellow)
                                   (rest moves))
                            board))
                        board)]
            board)
          board)))))

(defmethod board/game :tile-clicked
 [_ game-board tile-clicked]
  (update game-board :board human-player-move tile-clicked))
 
(defn start-game
  [widgets settings-player1 settings-player2 fn-exit-screen]
  (let [first-player-computer? (not= :human (second settings-player1))
        board {:game zig-zag
               :players (list settings-player1 settings-player2)
               :current-player :player1
               :fn-exit fn-exit-screen}]
    (-> widgets
        (assoc-in ["Dame" :board] board)
        (assoc-in ["Dame" :locked] first-player-computer?))))

(defn create-end-screen
  [widgets fn-create-main-menu]
  (-> widgets
      (gui/add-button "btn-back" "Back to Main Menu" {:x 500 :y 700 :z 30 :width 250 :color color-for-button :font-size 28 :group "back"})
      (gui/attach-event "btn-back" :mouse-clicked (fn [wdgs _]
                                                    (-> wdgs
                                                        (gui/remove-widget-group "back")
                                                        (assoc-in ["Dame" :big-text] nil)
                                                        fn-create-main-menu)))))

(defn create-play-mode-menu
  [widgets fn-create-main-menu]
  (-> widgets
      (gui/add-button "btn-human" "vs Human" {:x 260 :y 250 :z 3 :width 500 :color color-for-button :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-human" :mouse-clicked (fn [wdgs _]
                                                     (-> wdgs
                                                         (gui/remove-widget-group "play-menu")
                                                         (start-game [:player1 :human] [:player2 :human] (fn [w]
                                                                                                           (-> w
                                                                                                               (create-end-screen fn-create-main-menu)))))))
      (gui/add-button "btn-computer-easy" "vs Computer (easy)" {:x 260 :y 400 :z 3 :width 500 :color color-for-button :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-computer-easy" :mouse-clicked (fn [wdgs _]
                                                             (init-computer-player :player2)
                                                             (-> wdgs
                                                                 (gui/remove-widget-group "play-menu")
                                                                 (start-game [:player1 :human] [:player2 :easy] (fn [w] (-> w
                                                                                                                            (create-end-screen fn-create-main-menu)))))))
      (gui/add-button "btn-easy-vs-easy" "Nah I just watch" {:x 260 :y 550 :z 3 :width 500 :color color-for-button :font-size 28 :group "play-menu"})
      (gui/attach-event "btn-easy-vs-easy" :mouse-clicked (fn [wdgs _]
                                                            (init-computer-player :player1)
                                                            (init-computer-player :player2)
                                                            (-> wdgs
                                                                (gui/remove-widget-group "play-menu")
                                                                (start-game [:player1 :easy] [:player2 :easy] (fn [w]
                                                                                                                (-> w
                                                                                                                    (create-end-screen fn-create-main-menu)))))))))

(defn create-start-btn
  [widgets fn-create-main-menu]
  (-> widgets
      (gui/add-button "btn-start" "Start" {:x 400 :y 300 :z 3 :width 250 :color color-for-button :font-size 28 :group "menu"})
      (gui/attach-event "btn-start" :mouse-clicked (fn [wdgs _]
                                                     (-> wdgs
                                                         (gui/remove-widget-group "menu")
                                                         (create-play-mode-menu fn-create-main-menu))))))

(defn create-quit-btn
  [widgets]
  (-> widgets
      (gui/add-button "btn-quit" "Quit" {:x 400 :y 500 :z 3 :width 250 :color color-for-button :font-size 28 :group "menu"})
      (gui/attach-event "btn-quit" :mouse-clicked (fn [_ _]
                                                    (gui/close-window!)))))

(defn create-menu
  [widgets]
  (-> widgets
      (create-start-btn create-menu)
      create-quit-btn))

(defn -main
  ""
  []
  (gui/window! 400 400 1000 1000 "Dame" Color/white {java.awt.RenderingHints/KEY_ANTIALIASING java.awt.RenderingHints/VALUE_ANTIALIAS_ON
                                                     java.awt.RenderingHints/KEY_RENDERING java.awt.RenderingHints/VALUE_RENDER_SPEED})
  (let [board {:game game-start
               :players (list [:player1 :human] [:player2 :human])}]
    (gui/swap-widgets!
     (fn [wdgs]
       (-> wdgs
           (gui/add (board/create-board board))
           (assoc-in ["Dame" :locked] true)
           (create-menu))))))