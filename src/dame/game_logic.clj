(ns dame.game-logic)

(defn within-board? 
  [game [x y]]
  (let [y-max (count game)
        x-max (count (game 0))]
    (and (< y y-max) (< x x-max)
         (>= y 0) (>= x 0))))

(defn possible-moves-normal
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
  [game x y player]
  [[x y]])

(defn possible-moves
  "Returns a vector consisting of possible next moves"
  [game x y]
  (when (within-board? game [x y])
    (when-let [stone (seq (:player ((game y) x)))]
      (if (< (count stone) 2)
        (possible-moves-normal game x y (nth stone 0))
        (possible-moves-dame game x y (nth stone 0))))))

(defn next-game
  "Returns a new game with the transition [x0 y0] -> [x y] applied"
  [game [x0 y0] [x y]]
  game)