(ns lexer.main
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn ->TokenLiteral
  [t l]
  {:token t :literal l})

(defn lex-letter
  [string table]
  (let [literal (->> string
                     (take-while #(Character/isLetter ^Character %))
                     (apply str))
        token (or (get table literal)
                  :ident)]
    (->TokenLiteral token literal)))

(defn lex-digit
  [string _]
  (let [literal (->> string
                     (take-while #(Character/isDigit ^Character %))
                     (apply str)
                     (parse-long))]
    (->TokenLiteral :long literal)))

(defn lex-other
  [string table]
  (let [[a b] string
        token (or (get table (str a b))
                  (get table a)
                  :illegal)]
    (->TokenLiteral token (str a b))))
;;

(defn lexer
  [code-string tokens-table]
  (loop [[^Character a & _ :as cs] code-string
         acc (transient [])]
    (cond
      (nil? a) (->> (conj! acc (->TokenLiteral :eof ""))
                    persistent!)
      (Character/isWhitespace a) (recur (rest cs) acc)
      :else (let [token-literal (cond
                                  (Character/isLetter a) (lex-letter cs tokens-table)
                                  (Character/isDigit a) (lex-digit cs tokens-table)
                                  :else (lex-other cs tokens-table))
                  literal-length (-> token-literal :literal str count)
                  cs (->> (drop literal-length cs))]
              (recur cs (conj! acc token-literal))))))
;;

(def tokens-table
  (edn/read-string
   (slurp "src/tokens.edn")))

(defn -main
  ([& args]
   (let [args (str/join "\n" args)]
     (pp/pprint (lexer args tokens-table)))))

(comment
  (-main "let")
  (-main "= ")
  (-main "$$")
  (def x (apply str (repeat 100000 "let x = 4;\n x == 5 ? 1 : 0;")))
  (dotimes [n 10]
    (time (lexer x tokens-table))))
