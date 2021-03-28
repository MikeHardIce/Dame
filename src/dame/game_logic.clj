(ns dame.game-logic)

(defn- within-board?
  "Checks if the given coordinate is within the board,
   If so, returns true, otherwise nil" 
  [game [x y]]
  (let [y-max (count game)
        x-max (count (game 0))]
    (and (< y y-max) (< x x-max)
         (>= y 0) (>= x 0))))

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
      (if (= [cur-x cur-y] to)
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

(defn determine-possible-move
 "Computes the next possible coord using x-fun and y-fun.
  in case the coord is occupied with the oposite player,
  then apply both functions again. This means move along
  the diagonal once more if the tile is occupied." 
  [game x y x-fun y-fun player]
  (let [[x0 y0] [(x-fun x) (y-fun y)]
        player-on-stone (first (seq (:player (nth (nth game y0 nil) x0 nil))))]
    (if (and player-on-stone (not= player player-on-stone))
      [(x-fun x0) (y-fun y0)]
      [x0 y0])))

(defn possible-moves-normal
  "Hands back a vector of vectors containing the coordinates of the potential
   next moves of the stone at x y"
  [game x y player]
  (let [direction (if (= player :player1) -1 1)
        determine-next #(determine-possible-move game x y % (partial + direction) player)
        pos-moves [(determine-next inc) (determine-next dec)]
        moves (filter #(within-board? game %) pos-moves)]
    (filter #(let [x0 (first %)
                   y0 (second %)
                   stone (seq (:player ((game y0) x0)))]
               (not stone))
               moves)))

(defn possible-moves-dame
  "Hands back a vector of vectors containing the coordinates of the potential
   next moves of the dame (2 stones) at x y"
  [game x y player]
  [[x y]])

(defn possible-moves
  "Returns a vector consisting of possible next moves for the given stone/dame at x y"
  [game x y]
  (when (within-board? game [x y])
    (when-let [stone (seq (:player ((game y) x)))]
      (if (< (count stone) 2)
        (possible-moves-normal game x y (nth stone 0))
        (possible-moves-dame game x y (nth stone 0))))))

(defn remove-stones-on-path
  [game stones]
  (loop [game game
         stones stones]
    (if (seq stones)
      (let [[x y] (first stones)
            game (assoc-in game [y x] nil)]
        (recur game (rest stones)))
      game)))

(defn- transform-game
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