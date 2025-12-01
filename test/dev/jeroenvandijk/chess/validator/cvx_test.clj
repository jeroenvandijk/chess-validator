(ns dev.jeroenvandijk.chess.validator.cvx-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [dev.jeroenvandijk.chess.validator.clj :refer [start-fen]]
            [dev.jeroenvandijk.chess.validator.move-generator :refer :all]
            [dev.jeroenvandijk.chess.validator.tester :as tester]
            [clojure.set :as set]
            [clojure.string :as str]
            [dev.jeroenvandijk.chess.validator.cvm :as cvm]
            [dev.jeroenvandijk.chess.validator.cvx :as cvx]))


(defn chop-counters [fen]
  (->> (str/split fen #" ")
       (drop-last 2)
       (str/join " ")))


(defn chess-source [] 
  (cvx/generate) ;; Never an old version!
  (str "(do " (slurp (io/resource "dev/jeroenvandijk/chess/validator/chess.cvx")) ")"))

(defn initial-context []
  (let [ctx (cvm/create-ctx)]
    (.getResult (cvm/run-str ctx (chess-source)))  
    (cvm/eval ctx '(def current-state start-state))
    ctx))

;; move back to cvm

(require '[convex.clj]
         '[convex.cvm])

(defn any-result [ctx]
  (if-let [ex (convex.cvm/exception ctx)] 
    (throw (ex-info (str "error:" (.getMessage #_convex.core.cvm.exception.Failure/.getMessage ex)) {} ))
    (convex.clj/any (convex.cvm/result ctx))))

(defn state->fen [ctx]
  (any-result (cvm/eval ctx '(state->fen current-state))))

(defn move [ctx uci]
  (cvm/eval ctx (list 'do
                      (list 'def 'current-state (list 'move 'current-state uci))))
  ctx)

(defn valid-move?* [ctx from to promotion-piece]
  (try (any-result (cvm/eval ctx (list 'valid-move?* 'current-state from to promotion-piece)))
       (catch Exception e 

         (throw (ex-info "Error at " {:data [#_(state->fen ctx) from to promotion-piece]} e)))
       ))

(defn explain-move* [ctx from to promotion-piece]
  (any-result (cvm/eval ctx (list 'second (list 'explain-move* 'current-state from to promotion-piece)))))



#_(defn legal-moves* [])

;; TODO mapleaf to string
#_(.getResult (move (initial-context) "e2e4"))


(-> 
    (move (initial-context) "e2e4")
    (explain-move* 57 42 nil)
    ;state->fen
  )

(-> 
    (move (initial-context) "e2e4")
    #_(explain-move* 56 35 nil)
    (valid-move?* 56 35 nil)
    ;state->fen
  )

(let [ctx (initial-context)
      _ (->
         (move ctx "e2e4")
      ; (explain-move* 57 42 nil)
         )
      actual-fen (state->fen ctx)]

  actual-fen
  (count (legal-moves (fn [from to promotion-piece]
                        (valid-move?* ctx from to promotion-piece))
                      actual-fen)))



(deftest ucis
  (doseq [game
          (->> (clojure.string/split-lines (slurp "games.txt"))
               (take 10)
               #_(take 4000))]
    (println (str/join " " (tester/pgn->uci game)))
    (println)
    (print "G") (flush)
    (reduce (fn [acc uci]
              (let [[ctx board ucis] acc
                    ;_ (println "FEN " (tester/board->fen board))
                    ucis0 (conj ucis uci)
                    ;fen-before  (tester/board->fen board)

                    board0 (try
                             (tester/push-uci board uci)
                             (catch Exception e
                               (println "\nFollowing PGN gves an error at" uci "(step " (count ucis0) ")")
                               (println " " game)
                               (println)
                               (println "Extracted following UCI game")
                               (println " " (str/join " " ucis0))

                               #_(println "Fen at this point"
                                          fen-before)

                               (println "my fen")
                               (println (state->fen ctx))
                               (println)

                               (throw (ex-info (ex-message e)
                                               {} e))))
                    ;; TODO implement counters
                    expected-fen (chop-counters (tester/board->fen board0))
                    ;_ (println "Expected-fen" expected-fen)
                    expected-moves (tester/board->legal-moves board0)
                    ctx0 (try (move ctx uci)
                                #_(catch Exception e))
                    actual-fen (chop-counters (state->fen ctx0))
                    actual-moves (legal-moves (fn [from to promotion-piece]
                                                (valid-move?* ctx0 from to promotion-piece))
                                              actual-fen)]

                (testing "board state"
                  (is (= actual-fen expected-fen)))

                (testing "legal moves"
                  (is (= actual-moves expected-moves)))

                (if (and (= actual-fen expected-fen)
                         (= actual-moves expected-moves))
                  (do (print ".") (flush)
                      [ctx0 board0 ucis0])
                  (do
                    (println "After applying ucis:")
                    (println "  " (str/join " " ucis0))
                    (println)
                    (println "png:")
                    (println " " (str/join " " (take (count ucis0) (str/split game #" "))))
                    (println)

                    (when-not (= actual-fen expected-fen)
                      (println "Got wrong fen:")
                      (println "  Expected: " expected-fen)
                      (println "  Actual:   " actual-fen))

                    (when-not (= actual-moves expected-moves)
                      (println "For FEN:" expected-fen)
                      (println "Got wrong moves")
                      (println "There are " (count expected-moves) "legal moves, found "
                               (count (set/intersection expected-moves actual-moves)))
                      (when-let [missing (not-empty (set/difference expected-moves actual-moves))]
                        (println "  Missing: " missing))

                      (when-let [extra (not-empty (set/difference actual-moves expected-moves))]
                        (println "  Extra: " extra)))

                    (reduced acc)))))

            [(initial-context)
             (tester/fen->board start-fen)
             []]
            (tester/pgn->uci game))))


;; TODO below scenarios. Also go back through the git commits to find other interesting scenarios. 
;; - promotion capture of rook (cancels castling)
;; - rook capturing potential castling rook

;; No en passant when piece is pinned
;; 1. e4 e5 2. f4 Qh4+ 3. g3 Qe7 4. fxe5 Qxe5 5. d3 d5 6. Bf4 Qe6 7. e5 Bd6 8. d4 Bb4+ 9. Nc3 c5 10. a3 Ba5 11. dxc5 Ne7 12. b4 Bc7 13. Nf3 O-O 14. Bd3 f5


;; Wrong FEN when and extra move 
;; No en passant when piece puts king check. Piece is pinned! (lichess and python-chess agree)
;; 1. e4 e6 2. Nf3 h6 3. d4 Nf6 4. e5 Nd5 5. Bc4 Nb4 6. Bd2 N4c6 7. O-O a6 8. d5 exd5 9. Bxd5 Ne7 10. Nc3 c6 11. Bc4 d5 12. Bd3 Bg4 13. Be2 Bxf3 14. Bxf3 Ng6 15. Qe2 Nd7 16. Bh5 Ngxe5 17. Bf4 Qf6 18. Bxe5 Nxe5 19. g3 Bd6 20. Bg4 h5 21. Bh3 g5 22. f4 gxf4 23. gxf4 Qg7+ 24. Kh1 f6 25. fxe5 fxe5 26. Rg1 Qf7 27. Raf1 Qc7 28. Be6 Rf8 29. Qxh5+ Ke7 30. Rxf8 Rxf8 31. Rg6 Rf1+ 32. Kg2 Rf6 33. Rg7+ Kxe6 34. Rxc7 Bxc7 35. Qh3+ Kf7 36. Qh7+ Ke6 37. Qxc7 d4 38. Ne4 Rg6+ 39. Kh1 Kf5 40. Qf7+ Kxe4 41. Qxg6+ Kd5 42. Qd3 c5 43. c4+


;; En passant not possible when there is a piece in front? No => here it is possible 8/1p6/p7/2pkp3/3p4/3P4/PPP1Q2P/7K w - - 0 43


;; En passant is possible when king is in check 
;; 8/8/8/4kp1p/6pP/3B2K1/5P2/8 w - - 0 1     f2f4

;; Never mind it was about piece pinning.
