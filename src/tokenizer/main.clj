(ns tokenizer.main
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn ->token-literal
  [t l]
  {:token t :literal l})

(defn tokenize-letter
  [string table]
  (let [literal (->> string
                     (take-while #(Character/isLetter ^Character %))
                     (apply str))
        token (or (get table literal)
                  :ident)]
    (->token-literal token literal)))

(defn tokenize-digit
  [string _]
  (let [literal (->> string
                     (take-while #(Character/isDigit ^Character %))
                     (apply str)
                     (parse-long))]
    (->token-literal :long literal)))

(defn tokenize-other
  [string table]
  (let [[a b] string
        token (or (get table (str a b))
                  (get table a)
                  :illegal)]
    (->token-literal token (str a b))))
;;

(defn tokenizer
  [code-string tokens-table]
  (loop [[^Character a & _ :as cs] code-string
         acc (transient [])]
    (cond
      (nil? a) (-> (conj! acc (->token-literal :eof ""))
                   (persistent!))
      (Character/isWhitespace a) (recur (rest cs) acc)
      :else (let [token-literal (cond
                                  (Character/isLetter a) (tokenize-letter cs tokens-table)
                                  (Character/isDigit a) (tokenize-digit cs tokens-table)
                                  :else (tokenize-other cs tokens-table))
                  literal-length (-> token-literal :literal str count)
                  cs (drop literal-length cs)]
              (recur cs (conj! acc token-literal))))))
;;

(def tokens-table
  (edn/read-string
   (slurp "src/tokens.edn")))

(defn -main
  ([& args]
   (let [args (str/join "\n" args)]
     (pp/pprint (tokenizer args tokens-table)))))

(comment
  (-main "let")
  (-main "= ")
  (-main "$$")
  (def x (apply str (repeat 100000 "let x = 4;\n x == 5 ? 1 : 0;")))
  (dotimes [n 10]
    (time (tokenizer x tokens-table))))
