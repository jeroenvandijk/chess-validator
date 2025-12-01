(ns dev.jeroenvandijk.chess.validator.cvm
  (:refer-clojure :exclude [eval])
  (:require [clojure.java.io :as io])
  (:import [convex.core.cvm Context Address]
           [convex.core.init Init]
           [convex.core.crypto AKeyPair]
           [convex.core.lang Reader]))


(def genesis-address (Address/create 11))

(def KEYPAIRS
  (mapv
   AKeyPair/createSeeded
   [2 3 5 7 11 13 17 19]))

(def PEER_KEYS
  (mapv AKeyPair/.getAccountKey KEYPAIRS))

(def HERO_KEY (AKeyPair/.getAccountKey (first KEYPAIRS)))

(def base-state (Init/createBaseState HERO_KEY HERO_KEY PEER_KEYS))

(defn read-source [source]
  (Reader/read source))

(defn create-ctx 
  ([]
   (create-ctx Long/MAX_VALUE))
  ([juice-limit]
   (let [genesis-state base-state]
     (.. (Context/create genesis-state genesis-address juice-limit)
         (fork)))))

(defn eval-cvx-str'
  ([s]
   (eval-cvx-str' (create-ctx) s))

  ([ctx s]
   (.run ctx (read-source s))))

(defn eval-cvx-str
  ([s] (.getResult (eval-cvx-str' s)))
  ([ctx s] (.getResult (eval-cvx-str' ctx s))))

(defmacro eval-cvx' [ctx form]
  `(eval-cvx-str' ~ctx (pr-str ~form)))



(defmacro eval-cvx [form]
  `(eval-cvx-str (pr-str ~form)))


(defn eval [ctx form]
  (eval-cvx-str' ctx (pr-str form)))

(.getResult (eval-cvx-str' ":a"))
(comment
  (eval-cvx' (create-ctx) 2)
  (eval-cvx' (create-ctx) :bar)
  (let [form4  2]
    (.getResult (eval-cvx' (create-ctx) form4))))


(defn eval-forms [& forms]
  (.getResult
   (reduce (fn [acc form]
             (eval-cvx' acc form))
           (create-ctx)
           forms)))

(comment
  (eval-forms 
   '[(defn square [x] (* x x))
     (square 4)])

  (eval-forms
   '[(defn pow
       ([x]
        (recur x 2))
       ([x n]
        (loop [acc 1
               n n]
          (if (zero? n)
            acc
            (recur (* acc x) (dec n))))))
     (pow 16)])

  (eval-cvx (defmacro comment [& _body])) ;=> :ok
  (eval-cvx
   '(do (defmacro ->>
          [x & forms]
          (loop [x x, forms forms]
            (if forms
              (let [form (first forms)
                    threaded (if (list? form)
                               (concat form [x])
                               (list form x))]
                (recur threaded (next forms)))
              x)))
  
        ;(expand '(->> [1 2 3] (map inc) (map dec)))
        (->> [1 2 3] (map inc) (map dec))))

  :-)
  
(defn run-str [ctx s]
  (.run ctx (read-source s)))

(defn run [ctx form]
  (run-str ctx (pr-str form)))

(defn result [ctx form]
  (.getResult (run ctx form)))

(defn with-juice-used [ctx f]
  (let [before (.getJuiceAvailable ctx)
        _ (f)
        after (.getJuiceAvailable ctx)]
    (- before after)))


(defn juice-used [ctx form]  
  (with-juice-used ctx (fn [] (result ctx form))))

(defn load-context [ctx forms]
  (reduce
   (fn [ctx0 form]
     (let [ctx1 (try
                  (let [ctx1 (run ctx0 form)
                                        ;_ (.reviseJuiceLimit ctx0 Long/MAX_VALUE)
                        v (.getResult ctx1)]
                    #_(when (.isExceptional ctx1)
                        (let [ex (.getExceptional ctx1)]

                          (throw (ex-info (str "help me" (.getMessage ex))
                                          { ;:form-str form-str
                                        ;:ex ex
                                           :form form
                                           :meta (meta form)}))))

                    ctx1)
                  (catch Exception e
                    (throw (ex-info (ex-message e) { ;:form-str form-str
                                                    :x e
                                                    :form form
                                                    :meta (meta form)}
                                    e))))
                                        ;  v (.getValue ctx1)
           ]
       ctx1))
   ctx forms)
  )



(defn pp-str [x]
  (with-out-str (clojure.pprint/pprint x)))

(comment

  (transaction {:address address :keypair CHESS_KEYPAIR :source (str "(do" (slurp "convex-chess.clj") "\n)")})
;; Rules
;; - fn can't call itself via name, must use recur(?). Use reader conditional for this
;; - cannot use docstrings in `defn` use ^{:doc ...}
;; - cannot use ' in symbols
;; - (str nil) gives "nil"
;; -> (first []) doesn't work. For vectors you can use `get`, for sets you have to convert it to a vector first. E.g. (get (vec #{}) 0)

  (eval-cvx '(let [[a' b] [:a :b]]
               a'))

  (eval-cvx '(let [[a b] []]
               a))

  (eval-cvx '(let [[a & b] [1]]
               a))

  (eval-cvx '(let [[a b] [1 2 3]]
               a))

  (eval-cvx '(rem (int 2) (int 8)))

  ()

;; 

;; Can't use nested fns? Via juxt for instance?
;; fn* In expression

;; - cannot destructure a string like in clojure. Need to make it a vector first
  (eval-cvx (let [[a b] (vec "ab")]
              a))

;; - cannot use [... :as foo] in destructuring
  (eval-cvx (let [[a b :as foo] [1 2]]
              a))

  (eval-cvx 'mapcat)
  (eval-cvx 'map)
  (eval-cvx 'mapv)

  (eval-cvx '(contains-key? {:a 1} :a))
  (eval-cvx '(contains-key? #{:a} :a))

  (eval-cvx '(into {} [[1 2]]))
  (eval-cvx '(into {} (mapcat (fn [x] (map (fn [i] [i x]) [1 2 3 4])) [1])))
  (eval-cvx '(join [1 2 3] ""))

;; - #() doesn't always work
  (eval-cvx
   '(do (defn map-ranks [f ranks]
          (into {} #_(sorted-map) (mapcat (fn [rank]
                                            (map (fn [file]
                                                   [[file rank]
                                                    {[file (f rank)] false}])
                                                 [0 1 3 4 5 6 7 8] #_(range 0 8)))
                                          ranks)))
        (map-ranks #(- % 2) #_(fn [i] (- i 2)) [6])))

;; REVIEW go through it form by form? Maybe we rewrite-clj or something?

  (import '[convex.core.cvm Context Address]
          '[convex.core.init Init]
          '[convex.core.crypto AKeyPair]
          '[convex.core.lang Reader])

  (defn board-vec->board-str [board]
    (reduce (fn [[s cnt] i]
              (let [[s cnt] (str s)
                    (if (zero? (mod i 8))
                      [(str (when-not (zero? cnt)
                              cnt)
                            "/")
                       0]
                      [s cnt])]
                (if-let [piece (nth board i)]
                  [(str s cnt (piece->char piece)) 0]
                  [s (inc cnt)]))
              [nil 0])
            all-positions))
  (mod 57 8)

  (map (fn [i]
         (let [[file rank] [(rem i 8) (- 7 (quot i 8))]]
           (+ file (* rank 8)))) (range 64))

  (-get-in {} [:a :b])

  (get-in {:a {:b #{:c}}} [:a :b :c])
  (eval-cvx '(get-in {:a {:b #{:c}}} [:a :b :c]))

  (eval-cvx-str (str "(do" (slurp "convex-chess.clj") "\n)"))

  (.getResult (load-context (Context/create base-state genesis-address (* 1000 1000 100) #_Long/MAX_VALUE) (code-forms)))

  (do
    (def game-ctx (load-context (Context/create base-state genesis-address (* 1000 1000 100) #_Long/MAX_VALUE) (code-forms)))

    (.getResult (eval-cvx' game-ctx
                           '(state->fen (game  "e2e4 e7e5 f1c4 b8c6 g1f3 c6d4 d2d3 d4f3 d1f3 g8f6 h2h3 f8c5 a2a3 e8g8 c1e3 c5e3 f3e3 f8e8 e3f3 c7c6 b1c3 b7b5 c4b3 d8a5 e1g1 c8b7 c3e2 c6c5 f1d1 d7d6 c2c4 b5c4 d3c4 a8c8 d1d5 b7d5 e4d5 a5b6 e2c1 e5e4 f3f4 f6h5 f4g4 h5f6 g4f4 b6a5 b3c2 a5e1 g1h2 f6h5 f4d6 e1f2 c2e4 e8e4 c1d3 f2g3 h2g1 g3d3 a1f1 c8e8 d6d7 d3e3 g1h1 h5g3 h1h2 g3f1")))))

  (-> (validator/game  "e2e4 e7e5 f1c4 b8c6 g1f3 c6d4 d2d3 d4f3 d1f3 g8f6 h2h3 f8c5 a2a3 e8g8 c1e3 c5e3 f3e3 f8e8 e3f3 c7c6 b1c3 b7b5 c4b3 d8a5 e1g1 c8b7 c3e2 c6c5 f1d1 d7d6 c2c4 b5c4 d3c4 a8c8 d1d5 b7d5 e4d5 a5b6 e2c1 e5e4 f3f4 f6h5 f4g4 h5f6 g4f4 b6a5 b3c2 a5e1 g1h2 f6h5 f4d6 e1f2 c2e4 e8e4 c1d3 f2g3 h2g1 g3d3 a1f1 c8e8 d6d7 d3e3 g1h1 h5g3 h1h2 g3f1")
      validator/state->fen)

  (eval-cvx (str "a" (when nil nil) "a"))

  (.getResult (eval-cvx' game-ctx
                         (list 'board-vec->board-str (vec (repeat 64 nil)))))

  (.getResult (eval-cvx' game-ctx
                         (list 'board-vec->board-str (vec (repeat 64 nil)))))

  (eval-cvx '(str nil (str nil (str "foo"))))

  (eval-cvx '(str "foo" (when false "bar")))

  (eval-cvx '(get (vec #{}) 0))
  (eval-cvx '(str nil))

  (eval-cvx '(do (defn foo ([] (foo 1)) ([n] (inc n))) (foo)))

  (eval-cvx '(keys (filter (fn [[k v]] (= (mod v 2) 0)) {:a 1 :b 2})))

  (transaction {:address address :keypair CHESS_KEYPAIR
                :source
                "(#139/create-game)"})

  (def opponent-address 138)
  (def opponent-keypair (AKeyPair/createSeeded opponent-address))

  (defn opponent-transaction [source]
    (transaction {:address opponent-address
                  :keypair opponent-keypair
                  :source source}))

  (opponent-transaction "(#139/join-game 2)")

  (create-account opponent-keypair)

  (def response *1)

  (-> response :body (json/parse-string true) :value)

  (fund-account 138)

  (defn ->transaction [txn]
    (-> (transaction txn) :body (json/parse-string true) :result))

  (->transaction {:address address :keypair CHESS_KEYPAIR
                  :source
                  "(eval-as #139 *controller*)"})

  (->transaction {:address address :keypair CHESS_KEYPAIR
                  :source
                  "(call #140 (create-game))"})

  (->transaction {:address address
                  :keypair CHESS_KEYPAIR
                  :source
                  (slurp (clojure.java.io/resource
                          "dev/jeroenvandijk/validator/contract.clj"))})
)
