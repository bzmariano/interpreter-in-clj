(ns lexer.main
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn ->TokenLiteral
  [t l]
  {:token t :literal l})

(def tokens-table
  (edn/read-string (slurp "src/tokens.edn")))

(defmulti lexical-analysis
  (fn [[^Character a] _]
    (cond
      (Character/isLetter a) :letter
      (Character/isDigit a) :digit
      :else :other)))

(defmethod lexical-analysis :letter
  [string table]
  (let [literal (->> string
                     (take-while #(Character/isLetter ^Character %))
                     (apply str))
        token (or (get table literal)
                  :ident)]
    (->TokenLiteral token literal)))

(defmethod lexical-analysis :digit
  [string _]
  (let [literal (->> string
                     (take-while #(Character/isDigit ^Character %))
                     (apply str)
                     (parse-long))]
    (->TokenLiteral :long literal)))

(defmethod lexical-analysis :other
  [string table]
  (let [not-letter-or-digit? #(not
                               (or (Character/isLetterOrDigit ^Character %)
                                   (Character/isSpaceChar ^Character %)))
        [a b] (->> string
                   (take-while not-letter-or-digit?))
        token (or (get table (str a b))
                  (get table a)
                  :illegal)]
    (->TokenLiteral token (str a))))
;;

(defn lexer
  [code-string tokens-table]
  (loop [cs code-string
         acc (transient [])]
    (if (empty? cs)
      (->> (conj! acc (->TokenLiteral :eof ""))
           persistent!)
      (let [token-literal (lexical-analysis cs tokens-table)
            literal-length (-> token-literal :literal str count)
            cs-remainder (->> (drop literal-length cs)
                              (drop-while #(Character/isWhitespace ^Character %)))]
        (recur cs-remainder
               (conj! acc token-literal))))))
;;

(defn -main
  ([] [])
  ([& args]
   (let [args (str/join "\n" args)]
     (pp/pprint (lexer args tokens-table)))))

(comment
  tokens-table
  (-main "let")
  (-main "= ")
  (-main "let x = 4;\n x == 5 ? 1 : 0;")
  (-main "$$"))
