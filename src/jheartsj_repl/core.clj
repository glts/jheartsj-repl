(ns jheartsj-repl.core
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [jheartsj-repl.api :as j]))

;; Lexer

(def space? (partial contains? #{\space \tab}))
(def digit? (partial contains? (set "0123456789_")))
(def quote? (partial = \'))

(declare
  space
  digits
  sspace
  string
  other)

(defn space [s tokens]
  (let [[c :as more] (str/triml s)]
    (cond
      (digit? c) #(digits more tokens "")
      (quote? c) #(string more tokens)
      c          #(other more tokens)
      :else      tokens)))

(defn digits [s tokens curr]
  (if-let [[_ m [c :as more]] (re-find #"^(_?\d+)(.*)" s)]
    (cond
      (quote? c) #(string more (conj tokens [:digits (str curr m)]))
      (space? c) #(sspace more tokens (str curr m))
      c          #(other more (conj tokens [:digits (str curr m)]))
      :else      (conj tokens [:digits (str curr m)]))
    (conj tokens [:error "digits expected"])))

(defn sspace
  [s tokens curr]
  (let [[_ m [c :as more]] (re-find #"^(\s+)(.*)" s)]
    (cond
      (digit? c) #(digits more tokens (str curr m))
      (quote? c) #(string more (conj tokens [:digits curr]))
      c          #(other more (conj tokens [:digits curr]))
      :else      (conj tokens [:digits curr]))))

(defn string [s tokens]
  (if-let [[_ m [c :as more]] (re-find #"^'([^']*(?:''[^']*)*)'(.*)" s)]
    (let [rstr (str/replace m "''" "'")]
      (cond
        (digit? c) #(digits more (conj tokens [:string rstr]) "")
        (space? c) #(space more (conj tokens [:string rstr]))
        c          #(other more (conj tokens [:string rstr]))
        :else      (conj tokens [:string rstr])))
    (conj tokens [:error "String error"])))

(defn other [s tokens]
  (let [[_ m [c :as more]] (re-find #"^(.[.:]*)(.*)" s)]
    (cond
      (digit? c) #(digits more (conj tokens [:other m]) "")
      (quote? c) #(string more (conj tokens [:other m]))
      (space? c) #(space more (conj tokens [:other m]))
      c          #(other more (conj tokens [:other m]))
      :else      (conj tokens [:other m]))))

(defn lex
  "Lexes the tokens in string s and returns a vector of tokens,
   that is, pairs of the shape [:type token-string].
   
   The types recognised are :digits, :string, and :other.
   
   The lexing is lenient, proper checking is deferred to the parse
   phase. For example, a sequence of digits that eventually will
   overflow a long is still lexed as [:digits \"9999999...\"]."
  [s]
  {:pre [(re-matches #"[\p{Graph}\p{Blank}]*" s)]}
  (trampoline space s []))

; (lex "3  2 }. 7345 333336 'wha''t'")

;; Parser/evaluation

(defn eval-monad [m y]
  ((j/long-monad m) (j/long-array y)))

(defn eval-dyad [d x y]
  ((j/long-dyad d) (j/long-array x) (j/long-array y)))

; See http://www.jsoftware.com/help/dictionary/dicte.htm
(defn eval-stack [stack]
  (let [[e a1 a2 a3 & more] stack]
    (match [e a1 a2 a3]
      [[(:or :edge :lpar) _] [:verb f] [:noun y] any]
        (if any
          (conj more any [:noun (eval-monad f y)] e)
          (conj more [:noun (eval-monad f y)] e))
      [[(:or :edge :lpar :adv :verb :noun) _] [:verb _] [:verb f] [:noun y]]
        (conj more [:noun (eval-monad f y)] a1 e)
      [[(:or :edge :lpar :adv :verb :noun) _] [:noun x] [:verb f] [:noun y]]
        (conj more [:noun (eval-dyad f x y)] e)
      [[:lpar _] [(:or :conj :adv :verb :noun) _] [:rpar _] any]
        (if any
          (conj more any a1)
          (conj more a1))
      :else stack)))

(defn classify-other [[kind s]]
  (cond
    (or (j/long-monad s) (j/long-dyad s)) [:verb s]
    (= s "(") [:lpar "("]
    (= s ")") [:rpar ")"]
    :else [:unknown s]))

(defn classify [token]
  (let [[kind s] token]
    (cond
      (= kind :edge) token
      (= kind :digits) [:noun (j/long-array s)]
      (= kind :other) (classify-other token)
      :else [:unknown s])))

(defn parse
  "Parse a vector of tokens such as

   [[:digits \"3\"] [:other \"+\"] [:digits \"76 81\"]]

   and evaluate it using the usual rules of evaluation in J.
   When parse returns it has made a best effort to evaluate the
   presumed J 'statement' exhaustively."
  [tokens]
  (loop [tokens (into [[:edge true]] tokens)
         stack ()]
    (let [newst (eval-stack stack)]
      (if (= newst stack)
        (if (empty? tokens)
          (rest stack)
          (recur (pop tokens) (conj stack (classify (peek tokens)))))
        (recur tokens newst)))))

; (parse (lex "5 $ 76 81"))
; (parse (lex "? >: 14 * i. 4 5"))

;; REPL main

(defn prompt []
  (print "   ")
  (flush)
  (read-line))

(defn -main [& args]
  (println "<3 jheartsj <3 <3 <3")
  (loop [line (prompt)]
    (when line
      (when-let [result (try
                          (parse (lex line))
                          (catch Throwable t
                            (println "error:" (:cause (Throwable->map t)))))]
        (if (and (= 1 (count result)) (second (first result)))
          (when-let [output (j/display-array (second (first result)))]
            (println output))
          (println "Parse error" result)))
      (recur (prompt)))))
