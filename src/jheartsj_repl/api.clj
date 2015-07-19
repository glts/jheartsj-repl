(ns jheartsj-repl.api
  (:refer-clojure :exclude [long-array])
  (:require [clojure.string :as str])
  (:import [ch.glts.jheartsj LongArray LongMonad LongDyad Monads Dyads]))

(defn long-array
  "Returns a LongArray for the string a, or a itself if it is
   already an instance of LongArray."
  [a]
  (if (instance? LongArray a)
    a
    (let [ls (map #(Long/valueOf %)
                  (str/split (str/replace a "_" "-") #"\s+"))]
      (if (= 1 (count ls))
        (LongArray/scalar (first ls))
        (LongArray/of (clojure.core/long-array ls))))))

; (long-array "89 324 _242")
; (long-array "42")

(def monads
  {">:" (Monads/increment)
   "-"  (Monads/negate)
   "?"  (Monads/roll)
   "$"  (Monads/shape)
   "#"  (Monads/tally)
   ","  (Monads/ravel)
   "{." (Monads/head)
   "}." (Monads/tail)
   "i." (Monads/integers)})

(def dyads
  {"+"  (Dyads/plus)
   "-"  (Dyads/minus)
   "*"  (Dyads/times)
   "%"  (Dyads/divide)
   ","  (Dyads/append)
   "}." (.passive (Dyads/drop))  ; arguments inverted
   "{." (.passive (Dyads/take))  ; arguments inverted
   "$"  (Dyads/reshape)})

; (def adverbs
;   {"/"  (fn [dyad] )})

(def conjunctions
  {"\"" (fn [verb rank] (.withRank verb rank))})  ; currently only for monads

(defn long-monad [m]
  (if (instance? LongMonad m)
    m
    (when-let [f (monads m)]
      (fn [y] (.apply f y)))))

(defn long-dyad [d]
  (if (instance? LongDyad d)
    d
    (when-let [f (dyads d)]
      (fn [x y] (.apply f x y)))))

(defn display-array [a]
  (if-let [s (.. a toDisplayString (orElse nil))]
    (str/replace s "-" "_")))

; (display-array (:noun (second (eval-stack inp-1))))
; (display-array (:noun (peek (pop (pop (eval-stack inp-2))))))
; (display-array (:noun (second (eval-stack inp-3))))
