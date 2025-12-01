#?(:clj (ns dev.jeroenvandijk.chess.validator.cvx-compatibility
    (:refer-clojure :exclude [split cond map mapcat])
    (:require [clojure.core :as clojure])))


(defn contains-key? [m k]
  (contains? m k))

(defn fail [flag msg]
     (throw (ex-info (str "Error: " msg) {:flag flag})))

(defn split
  ^{:doc  "Splits a string by the given character separator, returning a Vector of substrings excluding the separator(s). Separator may be a character, Long codepoint value or a 1-character UTF-8 String."}
  [s sep]
  (assert (or (char? sep)
              (and (string? sep)
                   (= (count sep) 1))))
  (String/.split s (str sep)))

(defmacro cond [& args]
  (if (even? (count args))
    `(clojure/cond ~@args)
    `(clojure/cond ~@(butlast args) :else ~(last args))))

(defn map [f & colls]
  (apply clojure/map f colls))

(defn mapcat [f & colls]
  (apply clojure/mapcat f colls))

(defmacro for-loop [[i-sym i-val condition step] & body]
  `(loop [~i-sym ~i-val]
     (when ~condition
       ~@body
       (recur ~step))))

(defn slice [a start end]
  (clojure/subs a start end))


(defmacro switch 
  ^{:doc "Like case (see https://stirtok.github.io/convex-quick-ref/#_switch)"}
  [& args]
  `(clojure/case ~@args))
