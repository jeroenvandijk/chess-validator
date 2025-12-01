(ns dev.jeroenvandijk.chess.validator.move-generator
  (:require [dev.jeroenvandijk.chess.validator.clj 
             :as clj
             :refer [valid-move?* coords->uci fen->state ]]))


(defn legal-moves*
  ([state]
   (legal-moves* (partial valid-move?* state) state))

  ([f state]
   (into #{}
         (for [from (range 64)
               to (range 64)
               promotion-piece (if (and (or (<= 0 to 7)
                                            (<= 56 to 63))
                                        (contains? #{:bP :wP} (nth (:board state) from)))
                                 [:R :B :Q :N]
                                 [nil])
               :when (f from to promotion-piece)]
           (coords->uci [from to promotion-piece])))))

(clj/uci->from+to "b8c6")

(defn legal-moves 
  ([fen]
   (legal-moves* (fen->state fen)))
  
  ([f fen]
   (legal-moves* f (fen->state fen))))
