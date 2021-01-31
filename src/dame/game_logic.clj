(ns dame.game-logic)

(defn possible-moves
  "Returns a vector consiting of possible next moves"
  [game x y]
  [[x y]])

(defn next 
  "Returns a new game with the transition [x0 y0] -> [x y] applied"
  [game [x0 y0] [x y]]
  game)