(ns dev.jeroenvandijk.chess.validator.tester
  (:require [libpython-clj2.python :as py]
            [libpython-clj2.require :refer [require-python]]
            [dev.jeroenvandijk.chess.validator.clj :refer [coords->uci uci->coords valid-move?]
            ]
            [clojure.string :as str]))


(py/initialize!)
(require-python '[chess :as chess])
(require-python '[builtins :as py-builtins])


(defn fen->board [fen]
  (chess/Board fen))


(defn board->legal-moves [board]
  (->> (py/get-attr board "legal_moves")
       (map #(py/call-attr % "uci"))
       py/->jvm
       (into #{})))


(defn board->fen [board]
  (py/call-attr board "fen"))


(defn push-san [board san]
  (py/call-attr board "push_san" san)
  board)

(defn push-uci [board uci]
  (py/call-attr board "push_uci" uci)
  board)

(defn pgn->board [pgn]
  (let [board (chess/Board)]
    (doseq [san (str/split pgn #" ")]
      (push-san board san))
    board))

(defn pgn->fen [pgn]
  (board->fen (pgn->board pgn)))


(defn pgn->uci [pgn]
  (-> (pgn->board pgn)
      (py/get-attr "move_stack")
      (->> (map str))))


(comment
  (pgn->board "d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4")

  (pgn->uci "d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4")

  (board->fen (pgn->board "d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4"))
  ,)

(defn fen->legal-moves [fen]
  (board->legal-moves (fen->board fen)))

(defn pgn->legal-moves [pgn]
  (board->legal-moves (pgn->board pgn)))

(defn board->illegal-moves-indices [board]
  (let [legal (into #{} (map uci->coords) board)
        from (shuffle (range 64))
        to (shuffle (range 64))]
    (sequence
     (remove legal)
     (for [a from
           b to]
       [a b]))))

(defn fen->illegal-moves-indices [fen]
  (board->illegal-moves-indices (fen->legal-moves fen)))

(defn fen->illegal-moves [fen]
  (map coords->uci (fen->illegal-moves-indices fen)))

(defn pgn->illegal-moves-indices [pgn]
  (board->illegal-moves-indices (pgn->legal-moves pgn)))

(defn pgn->illegal-moves [pgn]
  (map coords->uci (pgn->illegal-moves-indices pgn)))


(count (fen->illegal-moves "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R")) ;=> 4074
(first (fen->illegal-moves "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R")) 




(defn fen->checkmate? [fen]
  (py/call-attr (fen->board fen) "is_checkmate"))



(comment

  (fen->legal-moves "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R");=> opening

;; Check mate
  (fen->legal-moves "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4");=> #{}

  (fen->checkmate? "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"));=> true










;; TODO add to deps.edn
#_(clojure.repl.deps/add-libs
   '{clj-python/libpython-clj {:mvn/version "2.026"}})

(comment


)
