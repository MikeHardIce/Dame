(ns dame.game-logic)

(defn within-board?
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
      (= y y-max) :player1
      (= y 0) :player2
      :else nil)))

(defn possible-moves-normal
  "Hands back a vector of vectors containing the coordinates of the potential
   next moves of the stone at x y"
  [game x y player]
  (let [direction (if (= player :player1) -1 1)
        pos-moves [[(inc x) (+ y direction)] [(dec x) (+ y direction)]]
        moves (filter #(within-board? game %) pos-moves)]
    (filter #(let [x0 (first %)
                   y0 (second %)
                   stone (seq (:player ((game y0) x0)))]
               (not= (first stone) player)) 
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

(defn- transform-game
  [game [x0 y0] [x y]]
  (let [new-pos ((game y) x)
        old-pos ((game y0) x0)
        player-from (->> old-pos :player first)
        player-to (->> new-pos :player first)]
    (if (not= player-from player-to)
      (let [old-row (assoc (game y0) x0 nil)
            new-row (assoc (game y) x old-pos)
            new-game (assoc game y0 old-row)
            new-game (assoc new-game y new-row)]
        new-game)
      game)))

(defn next-game
  "Returns a new game with the transition [x0 y0] -> [x y] applied"
  [game [x0 y0] to]
   (let [allowed-moves (possible-moves game x0 y0)
         within (filter #(= % to) allowed-moves)]
     (if (seq within)
      (transform-game game [x0 y0] to)
      game)))