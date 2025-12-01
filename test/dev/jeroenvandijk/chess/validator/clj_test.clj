(ns dev.jeroenvandijk.chess.validator.clj-test
  (:require [com.mjdowney.rich-comment-tests :as rfc]
            [clojure.java.classpath :as cp]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [dev.jeroenvandijk.chess.validator.clj :refer [coord->index fen->state index->coord move state->fen valid-move?]]
            [dev.jeroenvandijk.chess.validator.move-generator :refer [legal-moves*]]
            [dev.jeroenvandijk.chess.validator.tester :as tester]
            [clojure.set :as set]
            
            [clojure.string :as str]))


(defn find-file-on-classpath [f]
  (some (fn [base]
          (when (java.io.File/.isDirectory base)
            (let [f0 (clojure.java.io/file base f)]
              (when (.exists f0)
                f0))))
        (cp/classpath)))


(defn ns->file [n]
  (let [path (:file (meta (val (first (ns-publics n)))))
        f (io/file (or (io/resource path) (io/file path)))]
    (some-> (if (java.io.File/.exists f)
              f
              (find-file-on-classpath path))
            (java.io.File/.getCanonicalPath))))


(defn run-ns-tests [n]
  (let [n0 (find-ns n)]
  (rfc/run-file-tests!
   (ns->file n0)
   n0)))


(deftest rct-tests
  (run-ns-tests 'dev.jeroenvandijk.chess.validator.clj))

(def start-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")


(defn try-n-illegal-moves [acc0 n fen]  
  (reduce (fn [acc move]
            (let [outcome
                  (try
                    (valid-move? fen move)
                    (catch Exception e
                      [:error (ex-message e)]))]
              (is (= outcome false))
              (if (false? outcome)
                (inc acc)
                (do
                  (prn (list 'valid-move? fen move))
                  (println "After " acc " tests. Found illegal move for: " move " in Fen " (pr-str fen))
                  (reduced acc)))))
          acc0
          (cond->> (tester/fen->illegal-moves fen)
            n
            (take n))))


(deftest dont-allow-basic-illegal-moves
  (try-n-illegal-moves 0 nil start-fen))

;; Do this tests
(every? (fn [pos] 
          (= (-> pos index->coord coord->index) pos))
        (range 0 64))


(defn create-pgns [end-pgn]
  (reductions (fn [acc san]
                (str acc " " san))
              (str/split end-pgn #" ")))


#_(deftest pgns
  (doseq [game
          #_["d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4"
             "d4 Nc6 e4 e5 f4 f6 dxe5 fxe5 fxe5 Nxe5 Qd4 Nc6 Qe5+ Nxe5 c4 Bb4+"]
          (take 40 ;; otherwise takes forever

                (clojure.string/split-lines (slurp "games.txt")))]
    (reduce (fn [cnt pgn]
              (let [fen (pgn->fen pgn)
                    expected (tester/fen->legal-moves fen)

                    ;; TODO we need to take into account promotion etc as well.
                    expected (into #{} (map #(subs % 0 4)) expected)

                    actual (legal-moves fen)]


                ;; TODO Apply fen before and see if they correspond
                (is (= expected actual))
                (if-not  (= expected actual)
                  (do
                    (println "For fen:" (pr-str fen))
                    (println "There are " (count expected) "legal moves, found "
                             (count (set/intersection expected actual)))
                    (when-let [missing (set/difference expected actual)]
                      (println "Missing:")
                      (doseq [coord missing]
                        (println (str "  - " (pr-str coord) " => " (explain-move fen coord)))))

                    (println "Found extra" (set/difference actual expected))
                    (println "Failed after " cnt " moves")
                    (reduced cnt))
                  (inc cnt))))

            0
            (create-pgns game))))



;; If board get's in an invalid state with the FEN setup we can still recover for invalid promotions. But with a pgn setup we would completely diverge. We can simply tests this by generating the FEN for and after. 

(tester/pgn->uci "d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4")
(create-pgns (str/join " " (tester/pgn->uci "d4 d5 c4 c6 cxd5 e6 dxe6 fxe6 Nf3 Bb4+ Nc3 Ba5 Bf4")))

(defn chop-counters [fen]
  (->> (str/split fen #" ")
       (drop-last 2)
       (str/join " ")))

(deftest ucis
  (doseq [game
          (->> (clojure.string/split-lines (slurp "games.txt"))
               (take 10)
               #_(take 4000))]
    (println (str/join " " (tester/pgn->uci game)))
    (println)
    (print "G") (flush)
    (reduce (fn [acc uci]
              (let [[state board ucis] acc
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
                               (println (state->fen state))
                               (println)

                               (throw (ex-info (ex-message e)
                                               {} e))))
                    ;; TODO implement counters
                    expected-fen (chop-counters (tester/board->fen board0))
                    ;_ (println "Expected-fen" expected-fen)
                    expected-moves (tester/board->legal-moves board0)
                    state0 (try (move state uci)
                                #_(catch Exception e))
                    actual-fen (chop-counters (state->fen state0))
                    actual-moves (legal-moves* state0)]

                (testing "board state"
                  (is (= actual-fen expected-fen)))

                (testing "legal moves"
                  (is (= actual-moves expected-moves)))

                (if (and (= actual-fen expected-fen)
                         (= actual-moves expected-moves))
                  (do (print ".") (flush)
                      [state0 board0 ucis0])
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

            [(fen->state start-fen)
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
