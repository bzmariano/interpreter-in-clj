(ns lexer.main
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defrecord TokenLiteral [token literal])

(def tokens-table
  (edn/read-string (slurp "src/tokens.edn")))

(defmulti lexical-analysis
  (fn [[^Character a & _] _]
    (cond
      (Character/isLetter a) :letter
      (Character/isDigit a) :digit
      :else :other)))

(defmethod lexical-analysis :letter
  [string table]
  (let [literal (->> string
                     (take-while #(Character/isLetter ^Character %))
                     (apply str))]
    (if-let [token (get table literal)]
      (->TokenLiteral token literal)
      (->TokenLiteral :ident literal))))

(defmethod lexical-analysis :digit
  [string _]
  (let [literal (->> string
                     (take-while #(Character/isDigit ^Character %))
                     (apply str)
                     (parse-long))]
    (->TokenLiteral :long literal)))

(defmethod lexical-analysis :other
  [string table]
  (let [[a b] (->> string
                   (take-while
                    #(not
                      (or (Character/isLetterOrDigit ^Character %)
                          (Character/isSpaceChar ^Character %)))))
        double-char (str a b)]
    (if-let [token (get table double-char)]
      (->TokenLiteral token double-char)
      (if-let [token (get table a)]
        (->TokenLiteral token (str a))
        (->TokenLiteral :illegal (str a))))))
;;

(defn lexer
  [code-string tokens-table]
  (loop [input (seq code-string)
         result-acc []]
    (if (empty? input)
      result-acc
      (let [token-literal (lexical-analysis input tokens-table)
            literal-length (->> token-literal :literal str count)
            input-remainder (->> (drop literal-length input)
                                 (drop-while #(Character/isWhitespace ^Character %)))]
        (recur input-remainder
               (conj result-acc token-literal))))))
;;

(defn -main
  ([] [])
  ([& args]
   (let [args (when (seq? args)
                (str/join "\n" args))]
     (pp/pprint
      (lexer args tokens-table)))))

(comment
  tokens-table
  (-main "let")
  (-main "= ")
  (-main "const x = 4; x == 5 ? 1 : 0;")
  (-main "$$"))
