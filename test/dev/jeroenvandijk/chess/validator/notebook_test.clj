(ns dev.jeroenvandijk.chess.validator.notebook-test
  (:require [clojure.test :as test :refer :all]
            [nextjournal.clerk :as clerk]
            [dev.jeroenvandijk.chess.validator.clj :as validator :refer [valid-move?]]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]))


{:nextjournal.clerk/visibility {:code :hide :result :hide}}

^::clerk/no-cache
(deftype Expectation [m])

(defn expectation? [x]
  (instance? (class (->Expectation {})) x))

(expectation? (->Expectation {}))





(def move-viewer
  {:pred expectation?
   :transform-fn (comp clerk/mark-preserve-keys
                       (clerk/update-val #(.-m %)))
   :render-fn '(fn [data]
                 (let [{:keys [fen move actual expected line]}
                       (into {} (map (juxt key (comp :dev.jeroenvandijk.chess.validator./value val))) data)
                       correct? (= actual expected)
                       render (fn []
                                ;; TODO show before, transition, after
                                [nextjournal.clerk.render/with-dynamic-import
                                 {:module "https://esm.sh/@lichess-org/chessground@9.8.5"}
                                 (fn [module]
                                   (let [Chessground (aget module "Chessground")]
                                     [:<>
                                      [:a {:href (str "https://lichess.org/analysis/fromPosition/" (clojure.string/replace fen #" " "_"))} "Verify with Lichess"]
                                      [:style "
           .chessground { width: 250px   ;
                          height: 250px  ; }
           cg-board {background-color: #bfcfdd ; }"]
                                      ;; REVIEW how to put the chessboard next to each other?

                                      [:div.chessground {:ref
                                                         (fn [el]
                                                           (when el
                                                             (let [from (.slice move 0 2)
                                                                   to (.slice move 2 4)]
                                                               (js/console.log (str "Move " from " to " to))
                                                               (doto (Chessground el (clj->js {:fen fen}))
                                                                 (.move from to)
                                                                 (.set (clj->js {:lastMove [from to]}))))))}
                                       "chessboard"]
                                      [:div.chessground {:ref
                                                         (fn [el]
                                                           (when el
                                                             (Chessground el (clj->js {:fen fen}))))}
                                       "chessboard"]]))])]

                   [:<>
                    [:hr]
                    (reagent.core/with-let [*show (reagent.core/atom (not correct?))]
                      [:<>
                       (if correct?
                         (str "✅ expected " expected)
                         (str  "❌ expected " expected ", got " actual))
                       " at line " line 
                       [:span " "]
                        "(" [:a {:on-click #(reset! *show (not @*show))} (if @*show "Hide" "Show") ] ")"
                       [:pre (pr-str (list 'valid-move? fen move))]

                       (when @*show
                         [render])])

                    [:br]]))})

^::clerk/no-cache
(clerk/add-viewers! [move-viewer])


#_"Need to show html or stylesheets don't render"
^{::clerk/visibility {:code :hide
                      :result :show}}
(clerk/html
 [:<>
  [:link {:rel "stylesheet" :href "https://unpkg.com/@lichess-org/chessground@9.8.5/assets/chessground.base.css"}]
  [:link {:rel "stylesheet" :href "https://unpkg.com/@lichess-org/chessground@9.8.5/assets/chessground.brown.css"}]
  [:link {:rel "stylesheet" :href "https://unpkg.com/@lichess-org/chessground@9.8.5/assets/chessground.cburnett.css"}]])






#_(comment
    ;; Use system properties to turn on Clerk specific features
    (System/setProperty "dev.env.interface" "nextjournal.clerk"))



(defmacro assert-move [fen move expected]
  (if (= (System/getProperty "dev.env.interface")
         "nextjournal.clerk")
    (let [{line :line column :column} (meta &form)]
      `(->Expectation {:fen ~fen
                       :move ~move
                       :actual ~(try (validator/valid-move? fen move)
                                     (catch Exception e
                                       (str "Error: " (ex-message e)))
                                     )
                       :expected ~expected
                       :line ~line
                       :column ~column}))
    (let [inner `(clojure.test/deftest ~(symbol (str "chess-assertion-" (gensym)))
                   (is (= (validator/valid-move? ~fen ~move) ~expected)))]
      (with-meta inner (meta &form)))))


(defmacro assert-valid-move [fen move]
  (with-meta `(assert-move ~fen ~move true) (meta &form)))


(defmacro assert-invalid-move [fen move]
  (with-meta `(assert-move ~fen ~move false) (meta &form)))


(comment

  ;(clerk/build! {:paths ["development/src/user/notebooks/example_viewer.clj"]})
  )

{::clerk/visibility {:code :hide
                     :result :show}}



;; ## Valid moves

(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2e4") ;; pawn single push
(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2e3") ;; pawn single push
(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "b1c3") ;; knight L move
(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "a2a4") ;; pawn double push from start

(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "e7e5") ;
; pawn double push

(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "b8a6") 
;; knight L move
(assert-valid-move "rnbqkb1r/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3" "f1b5") 
;; bishop along diagonal
(assert-valid-move "rnbqkb1r/pppp1ppp/2n1p3/8/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3" "e4e5") 
;; pawn single push into empty square

(assert-valid-move "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3" "d5c6") 

(:ep (validator/fen->state "rnbqkbnr/pp1p1ppp/8/2pPp3/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3"))


;; pawn captures diagonally
(assert-valid-move "r3k2r/pppppppp/8/8/8/4R3/8/R3K2R w KQkq - 0 1" "e1d1") 


;; ### Castling

(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w KQkq - 0 1" "e1c1") 
;; White Queen side

(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/R3KBNR w Kkq - 0 1" "e1c1") 
;; White Queen side, not available


(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1" "e1g1") 
;; White King side


(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w Qkq - 0 1" "e1g1") 
;; White King side, not available



(assert-valid-move "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "e8g8") 
;; Black King side

(assert-invalid-move "rnbqk2r/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQq - 0 1" "e8g8") 
;; Black King side, not available


(assert-valid-move "r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "e8c8") 
;; Black Queen side


(assert-invalid-move "r3kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQk - 0 1" "e8c8") 
;; Black Queen side, not available


 
;; king steps aside, remaining legal
(assert-valid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1" "e1g1") 

;; en passant capture

(assert-valid-move "8/P7/8/8/8/8/8/k6K w - - 0 1" "a7a8q") 



;; promotion to queen



(assert-valid-move "k6K/8/8/8/8/8/p7/1B6 b - - 0 1" "a2b1q") 
;; promotion by capture


(assert-valid-move "4k3/8/8/8/8/8/8/4K3 w - - 0 1" "e1d1") 
;; king moves along rank
(assert-valid-move "4k3/8/8/8/8/8/8/R3K3 w - - 0 1" "a1a2") 
;; rook one step
(assert-valid-move "8/8/8/4p3/3P4/8/8/8 w - - 0 1" "d4d5") 
;; pawn single push

(assert-valid-move "8/8/8/4p3/3P4/8/8/8 w - - 0 1" "d4e5") 
;; pawn capture
(assert-valid-move "8/8/8/8/3P4/8/8/3p4 w - - 0 1" "d4d5") 
;; forward into empty square
(assert-valid-move "8/8/8/8/3P4/8/8/3p4 w - - 0 1" "d4d5") 
;; forward into empty square
(assert-valid-move "4k3/8/8/8/8/8/3P4/4K2N w - - 0 1" "h1f2") 
;; knight L move
(assert-valid-move "4k3/8/8/8/8/8/8/1B2K3 w - - 0 1" "b1a2") 
;; bishop diagonal





;; ## Invalid moves
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2e5") 
; pawn cannot move three squares
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2e1") 
; own piece on destination
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "b1b3") 
; knight does not move straight
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "g1e2") 
; destination occupied by own pawn (e2)
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "a2a5") 
; pawn cannot move three squares
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e1g1") 
; path blocked for castling (f1 bishop, g1 knight)
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "b8b6") 
; knight cannot move straight
(assert-invalid-move "rnbqkb1r/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3" "c1h6") 
; blocked by own piece (d2 pawn)
(assert-invalid-move "rnbqkb1r/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3" "e4e6") 
; pawn cannot move two when not on start
(assert-invalid-move "r3k2r/pppppppp/8/8/8/4R3/8/R3K2R w KQkq - 0 1" "e3e8") 
; moving pinned piece exposes own king
(assert-invalid-move "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w Qkq - 0 1" "e1g1") 
; no K right to castle kingside
(assert-invalid-move "rnbqk1nr/pppppppp/8/2b5/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1" "e1g1") 


(assert-invalid-move "rnbqkbnr/pppp1ppp/8/3Pp3/8/8/PPP1PPPP/RNBQKBNR w KQkq - 0 3" "d5e6")

; no en passant right
(assert-invalid-move "rnbqkbnr/pppp1ppp/8/3Pp3/8/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 3" "d5e6") 
; ep target must be e6

(assert-invalid-move "8/P7/8/8/8/8/8/k6K w - - 0 1" "a7a8") 
; must specify promotion piece

(assert-invalid-move "8/P7/8/8/8/8/8/k6K w - - 0 1" "a7a8=K") 
; cannot promote to king
(assert-invalid-move "8/8/8/8/8/4k3/8/4K3 w - - 0 1" "e1e2") 

; king would be adjacent to enemy king
(assert-invalid-move "4k3/8/8/8/8/8/1r6/4K3 w - - 0 1" "e1e2") 
; square e2 attacked by rook a2

(assert-invalid-move "4k3/8/8/8/8/8/8/R3K3 w - - 0 1" "a1h1") 
; rook path blocked by own king on e1
(assert-invalid-move "4k3/8/8/8/8/8/8/R3K3 w - - 0 1" "a1e1") 
; destination occupied by own king
(assert-invalid-move "8/8/8/3p4/3P4/8/8/8 w - - 0 1" "d4d6") 
; pawn cannot jump over piece
(assert-invalid-move "4k3/8/8/8/8/8/3P4/4K2N w - - 0 1" "h1h3") 
; knight cannot move straight
(assert-invalid-move "8/8/8/8/8/8/8/4K3 b - - 0 1" "e1e2") 
; not that side to move (black to move)
(assert-invalid-move "4k3/8/8/8/8/8/8/1B2K3 w - - 0 1" "b1b2") 
; bishop must move diagonally





;; ### Self - check examples

(assert-invalid-move "4k3/8/8/8/8/8/8/1n2K3 w - - 0 1" "e1d2") 

;; King cannot be on attack square of knight



(assert-valid-move "4k3/8/8/8/8/8/r2P4/4K3 w - - 0 1" "e1e2") 
;; Rook, buffered




;; TODO bishop, queen 

;; TODO https://chess.stackexchange.com/questions/1482/how-do-you-know-when-a-fen-position-is-legal?utm_source=chatgpt.com

 

(assert-invalid-move "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R w KQkq - 0 1" "e1g1")
; cannot castle in check (f1 attacked) 



(assert-invalid-move "rnbqkbnr/pppp1ppp/8/2b5/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1" "e1g1")
; cannot castle in check (f1 attacked) 



;; #### Pinning
 
(assert-invalid-move "rnbqkbnr/pppp1ppp/8/b7/8/8/PPPPP1PP/RNBQK2R w KQkq - 0 1" "d2d3")



(assert-valid-move "rnbqkbnr/pppp1ppp/8/8/1q5P/8/PPPPPPP1/RNBQK2R w KQkq - 0 1" "h4h5")
;; Pinning with queen

;; #### King in check

#_(assert-invalid-move "rnbqkbnr/p1pppppp/b7/8/8/8/PPPP1PPP/RNBQK2R w KQkq - 0 1" "e1g1")


