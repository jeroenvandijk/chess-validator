#?(:clj (ns dev.jeroenvandijk.chess.validator.clj-compatibility
          (:refer-clojure :exclude [comment ->> -> keep take-while take drop juxt neg? pos? repeat merge-with range not-empty not= partial])))

#?(:cvx
 (defn contains? [m k]
   (contains-key? m k)))

(defmacro comment [& _body])

(defmacro ->>
  ^{:doc "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (list? form)
                       (concat form [x])
                       (list form x))]
        (recur threaded (next forms)))
      x)))


(defmacro ->
  ^{:doc "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (list? form)
                       (concat (list (first form) x) (next form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))


(defn keep [f x]
  (filter identity (map f x)))


(defn take-while [f coll]
  (reduce (fn [acc x]
            (if (f x)
              (conj acc x)
              (reduced acc)))
          [] 
          coll))


(defn take [n coll]
  (first (reduce (fn [[acc n0] x]
                     (if (zero? n0)
                       (reduced [acc n0])
                       [(conj acc x) (dec n0)]))
                   [[] n]
                   coll)))

(defn drop [n coll]
  (first (reduce (fn [[acc n0] x]
                   (if (zero? n0)
                     [(conj acc x) n0]
                     [acc (dec n0)]))
                 [[] n]
                 coll)))

(defn juxt
  ([f]
   (fn
     ([] [(f)])
     ([x] [(f x)])
     ([x y] [(f x y)])))
  ([f g]
   (fn
     ([] [(f) (g)])
     ([x] [(f x) (g x)])
     ([x y] [(f x y) (g x y)]))))


(defn neg? [x]
  (< x 0))

(defn pos? [x]
  (> x 0))

(defn repeat [n x]
  (loop [acc ()
         idx n]
    (if (<= idx 0)
      acc
      (recur (conj acc x) (dec idx)))))


(defn merge-with
  ^{:doc "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."}
  [f & maps]
  (when-not (empty? (filter identity maps))
    (let [merge-entry (fn [m [k v]]
                        (if (#?(:clj contains? :cvx contains-key?) m k)
                          (assoc m k (f (get m k) v))
                          (assoc m k v)))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) m2))]
      (reduce merge2 maps))))


#?(:clj
   (defmacro loop-safe [bindings & body]
     `(let [max# 1000
            *v# (volatile! max#)]
        (loop ~bindings
          (if (zero? (vswap! *v# dec))
            (throw (ex-info "Reached threshold" {:i max#}))
            (do ~@body))))))


(defn range
  ([end]
   (range 0 end))
  ([start end]
   (range start end 1))
  ([start end step]
   (if (neg? step)     
     (loop #_loop-safe [acc []
            i start]
       (if (<= i end)
         acc
         (recur (conj acc i) (+ i step))))
     (loop #_-safe [acc []
                 i start]
       (if (>= i end)
         acc
         (recur (conj acc i) (+ i step)))))))


(defn not-empty [x]
  (when-not (empty? x)
    x))

(defn not= [a b]
  (not (= a b)))


(defn partial
  ^{:doc "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."}
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f arg1 x))
     ([x y] (f arg1 x y))
     ([x y z] (f arg1 x y z))
     ([x y z & args] (apply f arg1 x y z args))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f arg1 arg2 x))
     ([x y] (f arg1 arg2 x y))
     ([x y z] (f arg1 arg2 x y z))
     ([x y z & args] (apply f arg1 arg2 x y z args))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f arg1 arg2 arg3 x))
     ([x y] (f arg1 arg2 arg3 x y))
     ([x y z] (f arg1 arg2 arg3 x y z))
     ([x y z & args] (apply f arg1 arg2 arg3 x y z args))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))