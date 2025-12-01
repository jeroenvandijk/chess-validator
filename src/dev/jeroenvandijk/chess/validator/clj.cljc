#?(:clj
(ns dev.jeroenvandijk.chess.validator.clj
   (:refer-clojure :exclude [cond juxt partial take reverse drop map keep reductions subs mapcat take-while neg? pos? repeat range some not= not-empty #_NaN? case #_vals -> ->> cond-> cond->> comment merge-with])
   (:require
    [clojure.core :as clojure]
    [clojure.string :refer [join]]
    [clojure.set :refer [intersection
                         subset?
                         union]]
    [clojure.math :refer [ceil pow signum sqrt]]
    [dev.jeroenvandijk.chess.validator.cvx-compatibility :refer :all]
    [dev.jeroenvandijk.chess.validator.clj-compatibility :refer :all]
    )))


(defn char->numeric-value
  ^{:doc "Mimics Character/getNumericValue"}
  [ch]
  (let [i (- (int ch) 48)]
    (if (<= 0 i 9)
      i
      -1)))


(defn coord->file+rank [coord]
  (let [[file-ch rank-ch] (vec coord)]
    [(switch file-ch
             \a 0
             \b 1
             \c 2
             \d 3
             \e 4
             \f 5
             \g 6
             \h 7)
     (dec (char->numeric-value rank-ch))]))


(defn file+rank->coord [[file rank]]
  (str (switch file 
         0 \a 
         1 \b  
         2 \c 
         3 \d 
         4 \e 
         5 \f 
         6 \g 
         7 \h)
       (inc rank)))


(defn file+rank->index [[file rank]]
  (+ file (* rank 8)))

(defn index->file+rank [idx]
  [(rem idx 8) (quot idx 8)])


(defn coord->index [coord]
  (file+rank->index (coord->file+rank coord)))


(defn index->coord [idx]
  (file+rank->coord (index->file+rank idx)))


^:rct/test
(comment
    ;; Why do comments not work? Macro should be ok?
  #_(coord->index "d5")
  
  ;(coord->index "d6") ;=> 43
  ;(coord->index "c5") ;=> 34
  ;(coord->index "e5") ;=> 36

  ;(index->coord 50)
  )


(defn uci->coords [s]
  [(coord->index (slice s 0 2))
   (coord->index (slice s 2 4))])


(defn uci->from+to [uci]
  (let [uci0 (uci->coords uci)]
    (if (= (count uci) 5)
      (conj uci0 ({\q :Q
                   \n :N
                   \r :R
                   \b :B} (last uci)))
      (conj uci0 nil))))


^:rct/test
(comment
  (uci->from+to "f7f8r") ;=> [53 61 :R]
  ,)


(defn coords->uci [[from to promotion-piece]]
  (str (index->coord from)
       (index->coord to)
       ({:Q \q
         :N \n
         :R \r
         :B \b} promotion-piece)))


(def char->piece
  {\p :bP \n :bN \b :bB \r :bR \q :bQ \k :bK
   \P :wP \N :wN \B :wB \R :wR \Q :wQ \K :wK})


(def empty-board (vec (repeat 64 nil)))


(defn board-str->board-vec [rank-str]
  (loop [chars (vec rank-str)
         col 0
         row 7
         acc empty-board]
    (if (empty? chars)
      acc
      (let [[ch & tail] chars]
        (if (= ch \/)
          (recur tail 0 (dec row) acc)
          (if (< col 8)
            (if-let [piece (get char->piece ch)]
              (recur tail
                     (inc col)
                     row
                     (assoc acc (+ (* row 8) col) piece))
              (let [i (char->numeric-value ch)]
                (if (<= 1 i 8)
                  (recur tail
                         (+ col i)
                         row
                         acc)
                  (fail :ASSERT (str "Unexpected char " ch)))))
            (fail :ASSERT (str "Out of bounds: " col))))))))


(defn calculate-ep-from [side ep-coord]
  (let [[to-file to-rank] (coord->file+rank ep-coord)
        ep-index (file+rank->index [to-file to-rank])
        from-rank (switch side :b (inc to-rank) :w (dec to-rank))
        left-from-file (dec to-file)
        right-from-file (inc to-file)]
    (disj #{(when (<= 0 left-from-file)
              [(file+rank->index [left-from-file from-rank]) ep-index])
            (when (< right-from-file 8)
              [(file+rank->index [right-from-file from-rank]) ep-index])} 
          nil)))


(defn ->index-move [coords]
  [(coord->index (slice coords 0 2))
   (coord->index (slice coords 2 4))])


(def white-kingside-castling (->index-move "e1g1"))
(def white-queenside-castling (->index-move "e1c1"))

(def black-kingside-castling (->index-move "e8g8"))
(def black-queenside-castling (->index-move "e8c8"))


^:rct/test
(comment
  ;; E.g. see https://lichess.org/analysis/fromPosition/rnbqkbnr/pp1p1ppp/8/8/2p1p2/8/PPP1PPPP/RNBQKBNR_w_KQkq_c6_0_3#8
  (fen->state "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 3") ;=>> {:side :w :ep #{[34 43] [36 43]} ...}

  (:castling (fen->state "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 3")) ;=> #{[4 2] [4 6] [60 58] [60 62]}
  )


(def opponent
  {:b :w :w :b})


(def piece->color
  {:bP :b :bN :b :bB :b :bR :b :bQ :b :bK :b
   :wP :w :wN :w :wB :w :wR :w :wQ :w :wK :w})


(defn within-bounds? [[x y]]
  (and 
   (<= -7 x 7)
   (<= -7 y 7)))


(defn ray
  ([direction]
   (#?(:clj ray
       :cvx recur) [0 0] direction))

  ([pos direction]
   (let [[xd yd] direction]
     (loop [[x y] pos
            acc []]
       (let [pos [(+ x xd) (+ y yd)]]
         (if (within-bounds? pos)
           (recur pos (conj acc pos))
           acc))))
   ))



;; REVIEW why we can't do this?
   #_(->> (reductions (fn [[x y] [dx dy]]
                        [(+ x dx) (+ y dy)])
                      pos
                      (repeat 8 direction))
       (take-while within-bounds?)
       (drop 1))

(defn rays [directions]
  (mapcat ray directions)
  ;; sort not available in CVM, also not needed here. Should we add it?
  #_(sort-by (juxt second first) (mapcat ray directions)))


(def knight-deltas 
  [[2 -1]  [2 1]
   [1 -2]  [1 2]  
   [-1 -2] [-1 2]
   [-2 -1] [-2 1]])


(def king-deltas 
  [[1 -1] [1 0] [1 1]
   [0 -1] [0 1]
   [-1 -1] [-1 0] [-1 1]])


(def bishop-directions [[-1 -1] [-1 1] [1 -1] [1 1]])
(def rook-directions [[-1 0] [0 -1] [1 0] [0 1]])


(def bishop-deltas (rays bishop-directions))
(def rook-deltas (rays rook-directions))
(def queen-deltas
  (concat bishop-deltas rook-deltas)
  #_(->> (concat bishop-deltas rook-deltas)
                    #_(sort)
                    (vec)))


(def all-positions (vec (range 0 64)))

(defn within-board? [x y]
  (and (<= 0 x 7)
       (<= 0 y 7)))


(defn map-moves [pos deltas]
  (let [[x y] (index->file+rank pos)]
    (into #{} #_(sorted-set)
          (keep (fn [[xd yd]]
                  (let [x0 (+ x xd)
                        y0 (+ y yd)]
                    (when (within-board? x0 y0)
                      (file+rank->index [x0 y0]))))
                deltas))))


(defn map-positions
  ([deltas]
   (map-positions deltas all-positions))
  
  ([deltas positions]
   (into {} (map (fn [pos] [pos (map-moves pos deltas)]) positions))))


(def king-positions (map-positions king-deltas))
(def knight-positions (map-positions knight-deltas))
(def rook-positions (map-positions rook-deltas))
(def bishop-positions (map-positions bishop-deltas))
(def queen-positions (map-positions queen-deltas))


(comment
  (get knight-positions 38)
  (get-in possible-moves [:wN 38])
  (get-in capture-moves [:wN 38]))



(defn map-ranks [f ranks]
  (into {} #_(sorted-map)
        (mapcat (fn [rank]
                  (map (fn [file]
                         [(file+rank->index [file rank])
                          #{(file+rank->index [file (f rank)])}])
                       (range 0 8)))
                ranks)))


(def black-pawn-double-push (map-ranks (fn [rank] (- rank 2)) [6]))

(def black-pawn-positions (merge-with union
                                      (map-ranks dec (range 6 0 -1))
                                      black-pawn-double-push))


(def white-pawn-double-push (map-ranks (fn [rank] (+ rank 2)) [1]))

#_(def pawn-double-push (merge-with union black-pawn-double-push white-pawn-double-push))


(def white-pawn-positions (merge-with union
                                      (map-ranks inc (range 1 7))
                                      white-pawn-double-push))


(def possible-moves
  {:bP black-pawn-positions
   :bN knight-positions
   :bB bishop-positions
   :bR rook-positions
   :bQ queen-positions
   :bK king-positions
   :wP white-pawn-positions
   :wN knight-positions
   :wB bishop-positions
   :wR rook-positions
   :wQ queen-positions
   :wK king-positions})

(get-in possible-moves [:wB 5])
(get bishop-positions 5)


;; FIXME remove the first and last ranks
(def black-pawn-captures (map-positions [[-1 -1] [1 -1]]))
(def white-pawn-captures (map-positions [[-1 1] [1 1]]))


(def capture-moves 
  (assoc possible-moves 
         :wB bishop-positions
         :wR rook-positions
         :wQ queen-positions
         :wP white-pawn-captures
         :bB bishop-positions
         :bR rook-positions
         :bQ queen-positions
         :bP black-pawn-captures))


^:rct/test
(comment
  (def possible-moves'
    (update-vals possible-moves
                 (fn [v]
                   (into (sorted-map) (map (juxt (comp index->coord key)
                                                 (comp (fn [vs]
                                                         (set (map index->coord vs))) val))
                                           v))
                   )))


  (get-in possible-moves' [:wB "f1a5"])
  
  (get-in possible-moves' [:bP "e7"])   ;=> #{"e5" "e6"}

  ;; FIXME rfc doesn't support matching sets via #{}

  ;; Knight
  (get-in possible-moves' [:wN "b1"])   ;=>> #(contains? %  "c3")


  (coord->index "b1")                   ;=> 1
  (index->coord 1)                      ;=> "b1"
  (index->coord 7)                      ;=> "h1" => incorrect
  (coord->index "c3")                   ;=> 18
  
  (get-in possible-moves [:wN 1])       ;=> #{11 16 18}
  (get (map-positions knight-deltas) 1) ;=> #{11 16 18}
  
  )


^:rct/test
(comment

  
  (opponent (piece->color :bN)) 
  
  (valid-move? "8/8/8/8/8/4k3/8/4K3 w - - 0 1" "e1e2") ;=> false
  ,)


(defn find-king-positions [board]
  (reduce
   (fn [acc pos]
     ;; This can be faster by using destrucring. acc (if acc (asssoc (reduced (assoc acc :w pos)) {:w pos}))
     (switch (nth board pos)
             :wK (if (= (count acc) 1)
                   (reduced (assoc acc :w pos))
                   {:w pos})
             :bK (if (= (count acc) 1)
                   (reduced (assoc acc :b pos))
                   {:b pos})
             acc))
   {} 
   all-positions))


(defn castling->str [castling]
  (if (empty? castling)
    "-"
    (str
     (if (contains? castling white-kingside-castling)  \K "")
     (if (contains? castling white-queenside-castling) \Q "")
     (if (contains? castling black-kingside-castling)  \k "")
     (if (contains? castling black-queenside-castling) \q ""))))


(defn str->castling [s]
  (into #{} (keep (fn [p]
                    (switch p
                            \K white-kingside-castling
                            \Q white-queenside-castling
                            \k black-kingside-castling
                            \q black-queenside-castling
                            nil))
                  (vec s))))

;; Convex has split
(defn fen->state [fen]
  (let [[placement side castling ep-coord half full] (split fen " ")
        board (board-str->board-vec placement)
        side (switch side "b" :b :w)]
    {:board board
     :side side
     :king-positions (find-king-positions board)
     :ep (switch ep-coord "-" #{}
               (calculate-ep-from side ep-coord))
     :castling (str->castling castling)}))


#_(-> (fen->state "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3")
    :ep)


^:rct/test
(comment
  (-> (fen->state "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3")
      :king-positions) ;=> {:w 4 :b 60}
  ,)


(defn index->file [idx]
  (rem idx 8))

(defn index->rank [idx]
  (quot idx 8))


^:rct/test
(comment
  (index->file 0);=> 0
  (index->file 8);=> 0
  (index->file 62);=> 6

  (index->rank 8);=> 1
  ,)


(defn rank->index [r]
  (* r 8))


(defn direction [[x y] [x1 y1]]
  (let [dir-x (- x1 x)
        dir-y (- y1 y)]
    [dir-x dir-y]
    (cond
      (zero? dir-x)
      (cond (pos? dir-y) [0 1]
            (neg? dir-y) [0 -1])

      (zero? dir-y)
      (cond (pos? dir-x) [1 0]
            (neg? dir-x) [-1 0])

      (= (abs dir-x) (abs dir-y))
      [(long (signum dir-x))
       (long (signum dir-y))])))


^:rct/test
(comment
  (defn direction' [a b]
    (direction (coord->file+rank a) (coord->file+rank b)))
  
  (direction' "a1" "a2");=> [0 1]
  (direction' "a1" "b1");=> [1 0]

  (direction' "a1" "b2");=> [1 1]

  (direction' "a1" "h8");=> [1 1]
  (direction' "h8" "a1");=> [-1 -1]
  (direction' "a8" "h1");=> [1 -1]
  (direction' "h1" "a8");=> [-1 1]
  (direction' "a1" "c1") ;=> [1 0]

  ,)


(defn betweens [a b]
  (let [[x y] a]
    (when-let [dir (direction a b)]
      (not-empty (into []
                       (take-while (partial not= b)
                        (map (fn [[xd yd]]
                               [(+ x xd) (+ y yd)])

                             (ray dir)))
                       ;; REVIEW ->> doesn't really work
                       #_(->> (ray dir)
                           (map (fn [[xd yd]]
                                  [(+ x xd) (+ y yd)]))
                           (take-while (partial not= b))))))))


(defn between-tiles? [a b c]
  (let [a (coord->file+rank a)
        b (coord->file+rank b)
        c (coord->file+rank c)]    
    (contains? (set (betweens a b)) c)))


^:rct/test
(comment
  (defn betweens' [a b]
    (some->> (betweens (coord->file+rank a) (coord->file+rank b))
             (mapv file+rank->coord)))
  
  (betweens' "a1" "a2")                 ;=> nil
  (betweens' "a1" "a3")                 ;=> ["a2"]
  (betweens' "a1" "c1")                 ;=> ["b1"]

                                        ;(betweens' "a1" "h1") ;=> ["
  (betweens' "a1" "h8")
  (betweens' "h8" "a1")
  (between-tiles? "a1" "h1" "e1")       ;=> true
  (between-tiles? "a1" "a2" "a3")       ;=> false
  (between-tiles? "a1" "a3" "a2")       ;=> true

  (between-tiles? "a1" "b2" "b1")       ;=> false

  (between-tiles? "a1" "c3" "b2")       ;=> true

  )


(def tiles-between
  (reduce (fn [acc pos-a]
            (reduce 
             (fn [acc0 pos-b]
               (if-let [b (betweens (index->file+rank pos-a)
                                    (index->file+rank pos-b))]
                 (assoc acc0 [pos-a pos-b] (mapv file+rank->index b))
                 acc0))
             acc
             all-positions))
          {}
          all-positions)

  #_(into {} (for [pos-a all-positions
                 pos-b all-positions]
             (when-let [b (betweens (index->file+rank pos-a)
                                    (index->file+rank pos-b))]
               [[pos-a pos-b] (mapv file+rank->index b)]))))


^:rct/test
(comment
  (tiles-between [1 8]) ;=> nil
  (tiles-between [0 7]) ;=> [1 2 3 4 5 6]
  (get tiles-between [0 16]);=> [8]
  (get tiles-between [0 24]);=> [8 16]
  (get tiles-between [24 0]);=> [16 8]
  (get tiles-between [0 63]);=> [9 18 27 36 45 54]
  (get tiles-between [63 0]);=> [54 45 36 27 18 9]
)


(def direction-between
  (reduce
   (fn [acc pos-a]
     (reduce (fn [acc0 pos-b]
               (if-let [b (direction (index->file+rank pos-a)
                                     (index->file+rank pos-b))]
                 (assoc acc0 [pos-a pos-b] b)
                 acc0))
             acc
             all-positions))
   {}
   all-positions)
  #_
  (into {} (for [pos-a all-positions
                 pos-b all-positions]
             (when-let [b (direction (index->file+rank pos-a)
                                     (index->file+rank pos-b))]
               [[pos-a pos-b] b]))))


(defn some [f coll]
  (reduce (fn [_ x] 
            (when-let [y (f x)]
              (reduced y)))
          nil 
          coll))


(defn path-blocked? [board from to]
  (boolean (some board (tiles-between [from to]))))


^:rct/test
#_(comment
    ;; TODO add new tests
    (valid-castling? (fen->state "rnbqkbnr/p1pppppp/b7/8/8/8/PPPPPPPP/RNBQK2R w - - 0 1") 4 6 :w) ;=> false
    (valid-castling? (fen->state "rnbqkbnr/p1pppppp/b7/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1") 4 6 :w) ;=> true
    (valid-castling? (fen->state "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R w KQkq - 0 1") 4 6 :w) ;=> false
    )
(defn attack? [board side dir target]
  (loop [tiles (map file+rank->index
                    (filter (fn [[x y]] (within-board? x y))
                            (ray (index->file+rank target) dir)))]
    (when-not (empty? tiles)
      (let [[tile & tiles] tiles]
        (if-let [piece (nth board tile)]
          (if (= (piece->color piece) side)
            false
            (contains? (get-in capture-moves [piece tile]) target))
          (recur tiles))))))

;; TODO keep track of king position. 
;; - Worst case 7 attack-sources, but only one direction matching
;; - For one direction, max one full iteration of 8 tiles
(defn pinned? [state from to]
  (let [board (:board state)
        side (:side state)
        king-position (get-in state [:king-positions side])]
    (if (= king-position from)
      false
                                        ;(direction from to)
      (when-let [dir (direction-between [king-position from])]
        (let [board0 (assoc board
                            from nil to (nth board from))]
          (attack? board0 side dir king-position))))))



(defn check?0 [state #_{:keys [board king-positions side]}]
  (let [board (:board state)
        side (:side state)
        king-position (get-in state [:king-positions side])]
    ;; - Check knight attacks (max 8 positions)
    ;; - Check all directions
    (when king-position
      (or (let [opponent-knight (switch side :b :wN :bN)]
            (some (fn [pos]
                    (= (nth board pos) opponent-knight))
                  (get-in possible-moves [opponent-knight king-position])))
          (some (fn [dir]
                  (attack? board side dir king-position))
                king-deltas)))))


(defn check? [state #_{:keys [board side] :as state} from to]
  (let [board (:board state)
        side (:side state)
        king-positions (:king-positions state)
        king-position (get king-positions side)]
    (check?0 (assoc state 
                    :board (assoc board
                                  from nil
                                  to (nth board from))
                    :king-positions (assoc king-positions side
                                           (if (= king-position from)
                                             to
                                             king-position))))))


^:rct/test
(comment

  (defn pinned?' [state move]
    (apply pinned? state (take 2 (uci->from+to move))))
  ;; Move first pawn
  (-> (fen->state "rnbqkbnr/pppp1ppp/8/b7/8/8/PPPPP1PP/RNBQK2R w KQkq c6 0 3")
      (pinned? 8 24)) ;=> nil

  (-> (fen->state "rnbqkbnr/pppp1ppp/8/b7/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1")
      (pinned? 11 19)) ;=> true

  (-> (fen->state "rnbqkbnr/pppp1ppp/8/b7/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1")
      (pinned? 11 19)) ;=> true

  (-> (fen->state "rnbqkbnr/pppp1ppp/8/8/1q5P/8/PPPPPPP1/RNBQK2R w KQkq - 0 1")
      (pinned?' "h4h5")) ;=> false

  ,)


(defn execute-castling [state king-from king-to castle-from castle-to]
  (let [side (:side state)
        castling (:castling state)
        board (:board state)
        king-positions (:king-positions state)]
    (assoc state 
           :castling (switch side 
                             :w (disj castling white-queenside-castling white-kingside-castling)
                             :b (disj castling black-queenside-castling black-kingside-castling))
           :board (assoc board 
                         king-from nil
                         king-to (nth board king-from)
                         castle-from nil
                         castle-to (nth board castle-from))
           :king-positions (assoc king-positions side king-to))))


(defn explain-castling [state from to]
  (let [castling (:castling state)]
    (when (contains? castling [from to])
      (let [board (:board state)]
        (when-let [f (get {white-kingside-castling (fn []
                                               ;; FIXME path blocked doesn't check until to, so need 1 more
                                                     (cond

                                                       (path-blocked? board from 7) [false :castling-blocked #_:castling-white-kingside-blocked]
                                                       (check?0 state) [false :castling-in-check]
                                                       (check? state from 5) [false :castling-through-check]

                                                       :else
                                                       (let [state0 (execute-castling state from to 7 5)]
                                                         (if (check?0 state0)
                                                           [false :castling-through-check #_:castling-into-check]
                                                           [true  :castling-white-kingside state0]))))

                           white-queenside-castling  (fn []
                                                       (cond (path-blocked? board from 0) [false :castling-blocked]
                                                             (check?0 state) [false :castling-in-check]
                                                             (check? state from 3) [false :castling-through-check]

                                                             :else
                                                             (let [state0 (execute-castling state from to 0 3)]
                                                               (if (check?0 state0)
                                                                 [false :castling-through-check #_:castling-into-check]
                                                                 [true  :castling-white-queenside state0]))))

                           black-kingside-castling
                           (fn []
                             (cond (path-blocked? board from 63) [false :castling-blocked]
                                   (check?0 state) [false :castling-in-check]
                                   (check? state from 61) [false :castling-through-check]

                                   :else
                                   (let [state0 (execute-castling state from to 63 61)]
                                     (if (check?0 state0)
                                       [false :castling-through-check #_:castling-into-check]
                                       [true  :castling-black-kingside state0]))))

                           black-queenside-castling (fn []
                                                      (cond (path-blocked? board from 56) [false :castling-blocked]
                                                            (check?0 state) [false :castling-in-check]
                                                            (check? state from 59) [false :castling-through-check]

                                                            :else
                                                            (let [state0 (execute-castling state from to 56 59)]
                                                              (if (check?0 state0)
                                                                [false :castling-through-check #_:castling-into-check]
                                                                [true :castling-black-queenside state0]))))}
                          [from to])]
          (f))))))

(defn update-castling [state piece]
  (switch piece
    ;; Initial rook positions
    0 (update state :castling disj white-queenside-castling)
    7 (update state :castling disj white-kingside-castling)
    56 (update state :castling disj black-queenside-castling)
    63 (update state :castling disj black-kingside-castling)

    state))


(defn move-piece [state from to]
  (let [board (:board state)
        side (:side state)
        from-piece (nth board from)
        ;; Any piece can capture one of the rooks and then we need to update castling too
        state0          
        (update-castling
         (assoc state :board (assoc board
                                    from nil
                                    to from-piece))
         to)]
    ;; Update castling
    (switch from-piece
            :wK
            (let [state1 (assoc-in state0 [:king-positions side] to)]
              (update state1 :castling (fn [castling]
                                         (disj castling
                                               white-queenside-castling
                                               white-kingside-castling))))
            :bK
            (let [state1 (assoc-in state0 [:king-positions side] to)]
              ;; Bug?: (update {:castling #{1 2 3 4}} :castling disj 1 2) 
              (update state1 :castling (fn [castling]
                                         (disj castling 
                                               black-queenside-castling 
                                               black-kingside-castling))))
            :wR (update-castling state0 from)
            :bR (update-castling state0 from)
            state0)))


;; For the calculation of correct FEN we also need to check if piece is pinned. More efficient if we do it lazily
(defn explain-en-passant [state from to]
  (let [board (:board state)        
        from-piece (nth board from)
        ep (:ep state)]
    (when (and (contains? ep [from to])
               (contains? #{:wP :bP} from-piece))
      (let [captured-piece (file+rank->index [(index->file to)
                                              (index->rank from)])]
        (if (pinned? state from to)
          [false :pinned state]
          [true :en-passant (assoc state
                                   :ep nil
                                   :board (assoc board
                                                 from nil
                                                 to from-piece
                                                 captured-piece nil))])))))


^:rct/test
(comment
  
  (explain-move "8/1p6/p7/2pkp3/2Pp4/3Q4/PP5P/7K b - c3 0 43" "d4c3") ;=> :pinned

  ,)


(def white-double-push-ep
  (into {} (map (fn [file]
                  (let [from (file+rank->index [file 1])
                        to (file+rank->index [file 3])
                        ep (file+rank->index [file 2])]
                    [[from to] ep])) 
                (range 0 8))))

(def black-double-push-ep
  (into {} (map (fn [file]
                  (let [from (file+rank->index [file 6])
                        to (file+rank->index [file 4])
                        ep (file+rank->index [file 5])]
                    [[from to] ep]))
                (range 0 8))))


(def double-push-ep 
  (merge white-double-push-ep black-double-push-ep))


;; Pre-compute?
(defn double-push->ep-coords [push-from push-to]
  (let [ep-to (int (+ push-from (/ (- push-to push-from) 2)))
        left [(dec push-to) ep-to]
        right [(inc push-to) ep-to]]
    (switch (index->file ep-to)
      0 #{right}
      7 #{left}

      #{right left})))


^:rct/test
(comment
  
(double-push->ep-coords 8 24) ;=> #{[25 16]}
(double-push->ep-coords 10 26) ;=> #{[27 18] [25 18]}
(double-push->ep-coords 15 31) ;=> #{[30 23]}


(double-push->ep-coords 48 32) ;=> #{[33 40]}
(double-push->ep-coords 55 39) ;=> #{[38 47]}

  ,)

#_(moves 
       (split "e2e4 e7e5 g1f3 d7d6 f1c4 c8e6 d2d3 e6c4 d3c4 c7c5 e1g1 h7h6 b1c3 g8f6 c3d5 f6e4 f1e1 e4f6 d5f6 d8f6 d1d5 b8d7 d5b7 a8b8 b7a7 f8e7 c1d2 f6f5 c2c3 f5c2 a1b1 e8g8 e1c1 c2d3 b2b4 f8d8 b4c5 d6c5 b1b8 d8b8 c1d1 b8b1 a7a8 e7f8 a8a4 b1d1 a4d1 e5e4 f3e1 d3d6 h2h3 d6e5 a2a4 f8d6 f2f4" " "))

(defn explain-pawn-move* [state from to promotion-piece]
  (let [board (:board state)
        side (:side state)
        from-piece (nth board from)]
    (when (contains? #{:wP :bP} from-piece)
      ;; TODO reuse mapping from possible moves here?
      (cond (contains? double-push-ep [from to])
            (let [ep-options (into #{} (filter (fn [[ep-from _]]
                                                 (let [ep-piece (nth board ep-from)]
                                                   (and (contains? #{:wP :bP} ep-piece)
                                                         ;; Not same color
                                                        (not= from-piece ep-piece))))
                                               (double-push->ep-coords from to)))]
              [true :pawn-double-push (assoc state
                                             :ep ep-options
                                             :board
                                             (assoc board
                                                    from nil
                                                    to from-piece))])
            (or (<= 0  to 7)
                (<= 56 to 63))
            (if (contains? #{:Q :N :R :B} promotion-piece)
              ;; Need to update-castling for all captured pieces. Can we refactor this?
              [true :promotion 
               (update-castling
                (assoc state :board
                       (assoc board
                              from nil
                              to (keyword (str (name side) (name promotion-piece)))))
                to)]
              [false :invalid-promotion state])))))


;; 

;(explain-move "r1bqkbnr/ppp2ppp/2np4/4p3/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq -" "e4d3")

(defn explain-move* [state from to promotion-piece]
  (let [board (:board state)]
    (if-let [from-piece (nth board from)]
      (let [side (:side state)
            from-color (piece->color from-piece)]
        (if (= from-color side)
          (let [to-piece (nth board to)
                to-color (piece->color to-piece)]
            (if (= from-color to-color)
              [false :self-landing state]
              (let [options (if to-piece
                              capture-moves
                              possible-moves)]
                (if (contains? (get-in options [from-piece from]) to)
                  ;(get-in options [from-piece from to])
                  (if (path-blocked? board from to)
                    [false :blocked state]
                    (if (pinned? state from to)
                      [false :pinned state]
                      (if (check? state from to)
                        [false :check state]
                        (or (explain-pawn-move* state from to promotion-piece)
                            [true :normal (move-piece state from to)]))))
                  (or (explain-castling state from to)
                      (explain-en-passant state from to)
                      [false :impossible state])))))
          [false :wrong-color state]))
      [false :no-piece state])))


(defn move*
  ([state from to]
   (move* state from to nil))

  ([state from to promotion-piece]
   (let [ep-before (:ep state)
         [ok? description state] (explain-move* state from to promotion-piece)
         side (:side state)]

     ;; For SAN notation we would need to also calculate capture, check, checkmate etc.
     (if ok?
       (update
        (assoc state
               :side (switch side :w :b :b :w))
        :ep (fn [ep-after]
              (if (= ep-before ep-after)
                nil
                ep-after)))

       (fail :ASSERT (str "Invalid move " description))))))


(defn move [state uci]
  (apply move* state (uci->from+to uci)))


(def start-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


(defn moves 
  ([ucis]
   (moves (fen->state start-fen) ucis))

  ([state ucis]
   (reduce move state ucis)))


(defn piece->char [piece]
  (switch piece 
    :bR \r
    :bB \b
    :bN \n
    :bQ \q
    :bK \k
    :bP \p
    :wR \R
    :wB \B
    :wN \N
    :wQ \Q
    :wK \K
    :wP \P))


(def fen-positions 
  (mapv (fn [i]
          (let [[file rank] [(rem i 8) (- 7 (quot i 8))]]
            (+ file (* rank 8)))) all-positions))


(defn board-vec->board-str [board]
  (let [[s cnt] (reduce (fn [[s cnt] i]
                          (let [[s cnt]
                                (if (zero? (mod i 8))
                                  [(str s
                                        (if (zero? cnt)
                                          ""
                                          cnt)
                                        (if (= i 56) ;; No / for first row
                                          ""
                                          "/"))
                                   0]
                                  [s cnt])]
                            (if-let [piece (nth board i)]
                              [(str s
                                    (if (zero? cnt)
                                      ""
                                      cnt)
                                    (piece->char piece)) 0]
                              [s (inc cnt)])))
                        ["" 0]
                        fen-positions)]
    (str s 
         (if (zero? cnt)
           ""
           cnt))))


(defn state->fen [state]
  (let [board (:board state)
        side (:side state)
        castling (:castling state)
        ep (:ep state)]
    (str (board-vec->board-str board)
         " "
         (name side)
         " "
         (castling->str castling)
         " "
         (or (when-let [idx (get-in (vec ep) [0 1])]
               (index->coord idx))
             "-")
       ;; But we can just use https://lichess.org/analysis/fromPosition/rnb1kbnr/ppppqppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR_w_KQkq_d6_0_1
         " "
         "0" ;; TODO counter 1
         " "
         "0" ;; TODO counter 1
         )))


^:rct/test
(comment
  (-> (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
      (state->fen))

  (-> (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
      (state->fen))


  ;; Piece blocked?
  (valid-move? start-fen "f1a5") ;=> false

  (valid-move? start-fen "f1a5") ;=> false

  (explain-move start-fen "c3d3") ;=> :no-piece
  (explain-move start-fen "b7b5") ;=> :wrong-color
  (explain-move start-fen "h1h2") ;=> :self-landing

  (explain-move start-fen "h1h3") ;=> :blocked
  (explain-move start-fen "h1a3") ;=> :impossible

  (explain-move "rnbqk1nr/pp4pp/2p1p3/b7/3P4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 4 7" "c3b1") ;=> :pinned
  (explain-move "rnbqk1nr/pp4pp/2p1p3/8/1b1P4/5N2/PP2PPPP/RNBQKB1R w KQkq - 2 6" "e1d2") ;=> :check

  ;; King still in check
  (explain-move "rnbqkbnr/pp3ppp/2p1P3/8/3P4/8/PP2PPPP/RNBQKBNR b KQkq - 0 4" "e8d7") ;=> :check

  (explain-move "rnbqkbnr/pp3ppp/2p1P3/8/3P4/8/PP2PPPP/RNBQKBNR b KQkq - 0 4" "f7e6") ;=> :normal

  (explain-move "rnbqk1nr/pp4pp/2p1p3/8/1b1P4/5N2/PP2PPPP/RNBQKB1R w KQkq - 2 6" "c1d2") ;=> :normal

  ;; No knight attack
  (explain-move "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2" "e1e2") ;=> :normal

  ;; Knight catches queen 
  (explain-move "r1bqkbnr/pppp2pp/2n5/4Q3/4P3/8/PPP3PP/RNB1KBNR b KQkq - 3 7" "c6e5")
  ;=> :normal

  ;; King is never pinned
  (explain-move "r1bqkbnr/pppp2pp/2n5/4Q3/4P3/8/PPP3PP/RNB1KBNR b KQkq - 3 7" "e8f7") ;=> :normal

;; TODO need to implement move generation with promotion piece
  (explain-move "R6r/5P1p/2P2n1k/4p3/8/2P5/1P2BPPP/3QK1NR w K - 1 22" "f7f8r") ;=> :promotion

  (explain-move "r1b1kbnr/ppp1pppp/n7/4q3/8/2N5/PPPPBPPP/R1BQK1NR w KQkq - 4 5" "h2h3") ;=> :normal

  (explain-move start-fen "b2b4") ;=> :pawn-double-push

  (explain-move start-fen "f1a5") ;=> :impossible

  ;; Castling
  (explain-move "r3kbnr/pbp1pppp/p3q3/8/3P4/2N1BN2/PPP2PPP/R2QK2R w KQkq - 4 9" "e1g1") ;=> :castling-white-kingside

  (explain-move "r1b1kbnr/p1p1pppp/p3q3/8/3P4/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 8" "e1g1") ;=> :castling-in-check

  (explain-move "r3kbnr/pbp1pppp/p3q3/8/3P4/2N1BN2/PPP2PPP/R2QK2R w KQkq - 4 9" "e1c1") ;=> :castling-blocked
  (explain-move "r1bq1rk1/pppp1ppp/5n2/4p3/2B1P3/P2PbQ1P/1PP2PP1/RN2K2R w KQ - 0 9" "e1c1") ;=> :castling-blocked

  (explain-move "r3kb1r/1Qp1pppp/p4n2/8/6q1/2N1B3/PPP3PP/R4RK1 b kq - 0 13" "e8c8") ;=> :castling-through-check

  (explain-move "rn2kb1r/pp3pp1/3p1q1p/2pQp3/2P5/5N2/PPP2PPP/R1B1R1K1 b kq - 1 11" "e8c8") ;=> :castling-blocked

  (explain-move "r1bqkb1r/ppp1pppp/2n2n2/3p4/3P4/5N1P/PPP1PPP1/RNBQKB1R w KQkq - 3 4" "e1g1") ;=> :castling-blocked

  ;; en passant
  (explain-move "6k1/3n1pp1/3b3p/2p1q3/P1P1pP2/2P4P/3B2P1/3QN1K1 b - f3 0 28" "e4f3") ;=> :en-passant

  ;; Promotion
  (explain-move "6k1/6p1/7p/P2P4/8/2P4P/4p1PK/8 b - - 0 45" "e2e1q") ;=> :promotion

;; Invalid knight manoevers.... confused with en passant
  (explain-move "r2q1rk1/p1pn3p/1p2p1pb/4PpN1/3PN3/4Q3/PP3PPP/R4RK1 w - f6 0 16" "g5f6") ;=> :impossible

  (explain-move "r2q1rk1/p1pn3p/1p2p1pb/4PpN1/3PN3/4Q3/PP3PPP/R4RK1 w - f6 0 16" "g5e6") ;=> :normal

  (explain-move "r4rk1/ppp2ppp/3b4/5Bq1/PnPp2n1/1P1P2P1/4R2P/5RK1 b - c3 0 23" "b4c3") ;=> :impossible

  (explain-move "r2qkb1r/pp3ppp/2n5/2pp4/3PpPn1/PPP1P1PN/7P/RNB1KB1R b KQkq f3 0 9" "g4f3") ;=> :impossible
  )


(defn valid-move?*
  ([state from to]
   (valid-move?* state from to nil))

  ([state from to promotion-piece]
   (first (explain-move* state from to promotion-piece))))


^:rct/test
(comment
  
  (valid-move? "rnbqkbnr/pppp1ppp/8/b7/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1" "d2d3") ;=> false
  (valid-move? "4k3/8/8/8/8/8/r2P4/4K3 w - - 0 1" "e1e2") ;=> true
  (valid-move? "4k3/8/8/8/8/8/8/1n2K3 w - - 0 1" "e1d2") ;=> false
  #_(valid-move? "8/P7/8/8/8/8/8/k6K w - - 0 1" "a7a8=Q") ;=> true
  (valid-move? "4k3/8/8/8/8/8/8/1B2K3 w - - 0 1" "b1a2") ;=> true
  

  (valid-move? "4k3/8/8/8/8/8/8/R3K3 w - - 0 1" "a1h1") ;=> false
  

  (valid-move? "rnbqkb1r/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3" "c1h6")

  (valid-move? "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2e4"))


(defn valid-move? [fen uci]
  (let [state (fen->state fen)
        [from to promotion-piece] (uci->from+to uci)]
    (valid-move?* state from to promotion-piece)))


(defn explain-move [fen uci]
  (let [state (fen->state fen)
        [from to promotion-piece] (uci->from+to uci)]
    (second (explain-move* state from to promotion-piece))))


(def start-state (fen->state start-fen))


(defn game [uci-str]
  (reduce move start-state (do ;take 1 
                                 (split uci-str \space))))


