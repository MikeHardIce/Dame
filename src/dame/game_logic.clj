(ns dame.game-logic)

(defn- within-board?
  "Checks if the given coordinate is within the board,
   If so, returns true, otherwise nil"
  [game [x y]]
  (let [y-max (count game)
        x-max (count (game 0))]
    (and (< y y-max) (< x x-max)
         (>= y 0) (>= x 0))))

(defn- within-distance?
  [x0 y0 [x y] len]
  (let [diff-x (Math/abs (- x0 x))
        diff-y (Math/abs (- y0 y))]
    (or (<= diff-x len) (<= diff-y len))))

(defn is-on-border?
  "Checks if the given coordinate is on one of the player's border
   If so, returns the key of the player the border/side belongs to,
   otherwise nil"
  [game [x y]]
  (let [y-max (count game)]
    (cond
      (= y (dec y-max)) :player1
      (= y 0) :player2
      :else nil)))

(defn get-all-tiles-on-the-way
  "Returns all the tiles of the game that lay on the path
   from [x0 y0] to [x y] not including [x0 y0] and [x y]"
  [game [x0 y0] [x y :as to]]
  (let [direction-x (if (< x0 x) 1 -1)
        direction-y (if (< y0 y) 1 -1)]
    (loop [tiles []
           cur-x (+ x0 direction-x)
           cur-y (+ y0 direction-y)]
      (if (or (= [cur-x cur-y] to) (< cur-x 0) (< cur-y 0))
        tiles
        (recur (conj tiles {:player (:player ((game cur-y) cur-x)) :coord [cur-x cur-y]}) (+ cur-x direction-x) (+ cur-y direction-y))))))

(defn stones-on-the-way
  "Returns the stones belonging to the given player
   that lay in between from and to.
   Default uses the player 'from' belongs to"
  ([game [x0 y0 :as from] to] (stones-on-the-way game from to (first (:player ((game y0) x0)))))
  ([game from to player]
   (let [tiles (filter #(and (seq (:player %))
                             (= (first (:player %)) player)) (get-all-tiles-on-the-way game from to))
         tiles (map #(:coord %) tiles)]
     (vec tiles))))

;;f (x) = -x + (y0 + x0)
;;f (x) = x + (y0 - x0)

(defn increase-circle
  [x0 y0 points]
  (loop [pnts points
         next-points []]
    (if (seq pnts)
      (let [point (first pnts)
            x (nth point 0)
            y (nth point 1)
            direction-x (if (pos? (- x0 x)) -1 1)
            direction-y (if (pos? (- y0 y)) -1 1)
            new [(+ x direction-x)
                 (+ y direction-y)]]
        (recur (rest pnts) (conj next-points new)))
      next-points)))

(defn all-diagonal-moves
  [x y board-length]
  (let [path (for [xn (range 0 board-length)]
               [[xn (+ xn (- y x))] [xn (- (+ y x) xn)]])
        path (mapcat identity path)
        ;; filter out all y smaller than 0
        path (filter #(>= (nth % 1 -1) 0) path)]
    path))

(defn possible-moves-next
  "Hands back a vector of vectors containing the coordinates of the potential
   next moves of the stone at x y"
  [game x y player distance directions]
  (println x " " y " D: " distance)
  (let [diag-moves (all-diagonal-moves x y (count game))
        pos-moves (filter #(within-distance? x y % distance) diag-moves)
        pos-moves (filter #(within-board? game %) pos-moves)
        ;; retrieve all opponent stones on the path
        opponent-stones (filter #(let [x0 (first %)
                                       y0 (second %)
                                       pl (:player (nth (nth game y0) x0))]
                                   (and (seq pl) (not= player (first pl)))) pos-moves)
        opponent (if (= player :player1) :player2 :player1)
        ;; get the closest opponent stones (the ones where there are no other opponent stones on the way)
        opponent-stones (filter #(not (seq (stones-on-the-way game [x y] % opponent))) opponent-stones)
        ;; remove all tiles that come after opponent stones
        pos-moves (filter #(not (seq (stones-on-the-way game [x y] % opponent))) pos-moves)
        ;; get the tiles behind the opponent stones
        outer-circle (increase-circle x y opponent-stones)
        pos-moves (concat pos-moves outer-circle)
        pos-moves (filter #(within-board? game %) pos-moves)
        ;; remove all tiles that come after own stones 
        pos-moves (filter #(not (seq (stones-on-the-way game [x y] %))) pos-moves)
        ;; remove all tiles on the wrong direction (only dame has pos y and neg y direction at the same time)
        pos-moves (filter (fn [elem] 
                            (let [diff-y (- (second elem) y)
                                  dirs (map #(* diff-y %) directions)]
                              (some #(>= % 0) dirs))) pos-moves)]
    ;; remove all tiles that contain a stone
    (vec (filter #(let [x0 (first %)
                        y0 (second %)
                        stone (seq (:player ((game y0) x0)))]
                    (not stone))
                 pos-moves))))

(defn possible-moves
  "Returns a vector consisting of possible next moves for the given stone/dame at x y"
  [game x y]
  (when (within-board? game [x y])
    (when-let [stone (seq (:player ((game y) x)))]
      (if (< (count stone) 2)
        (possible-moves-next game x y (nth stone 0) 1 (if (= (nth stone 0) :player1) [-1] [1]))
        (possible-moves-next game x y (nth stone 0) (count game) [-1 1])))))

(defn remove-stones-on-path
  [game stones]
  (loop [game game
         stones stones]
    (if (seq stones)
      (let [[x y] (first stones)
            game (assoc-in game [y x] nil)]
        (recur game (rest stones)))
      game)))

(defn get-winner 
  "Determines the winner of the game.
   nil -> no winner
   :player1 -> player 1 won
   :player 2 -> player 2 won"
  [game]
  nil)

(defn- transform-game
  "Executes the move from x0 y0 to x y"
  [game [x0 y0] [x y]]
  (let [new-pos ((game y) x)
        old-pos ((game y0) x0)
        player-from (->> old-pos :player first)
        opponent (if (= player-from :player1) :player2 :player1)
        opponent-stones-in-way (stones-on-the-way game [x0 y0] [x y] opponent)
        game (remove-stones-on-path game opponent-stones-in-way)
        player-to (->> new-pos :player first)]
    (if (not= player-from player-to)
      (let [new-game (assoc-in game [y0 x0] nil)
            new-game (assoc-in new-game [y x] old-pos)]
        new-game)
      game)))

(defn- handle-stone-upgrade
  "Upgrade the players stone, if the stone
   isn't already upgraded and if the stone is on
   the opponents border"
  [game [x y :as tile]]
  (if (is-on-border? game tile)
    (let [stones (:player (nth (game y) x))
          player (first stones)]
      (if (and player (= (count stones) 1))
        (assoc-in game [y x :player] [player player])
        game))
    game))

(defn next-game
  "Returns a new game with the transition [x0 y0] -> [x y] applied"
  [game [x0 y0] to]
  (let [allowed-moves (possible-moves game x0 y0)
        within (filter #(= % to) allowed-moves)]
    (if (seq within)
      (handle-stone-upgrade (transform-game game [x0 y0] to) to)
      game)))