(ns lexer.main-test
  (:require
   [clojure.test :refer [deftest testing is run-tests]]
   [clojure.edn :as edn]
   [lexer.main :as main]))

#_(run-tests)

(def tokens-table
  (edn/read-string (slurp "src/tokens.edn")))

(def valid-code-string
  "const x = 1;
  func add(a, b) {
    return a + b;
  }
  if(x > 10) {
    return x;
  } else {
    return add(x, 10);
  }")

(deftest lexer-valid-chars
  (testing "Testing lexer with all allowed chars"
    (is (main/lexer valid-code-string tokens-table)
        [{:token :const :literal "const"}
         {:token :ident :literal "x"}
         {:token :assign :literal "="}
         {:token :long :literal 1}
         {:token :semi-colon :literal ";"}

         {:token :func :literal "func"}
         {:token :ident :literal "add"}
         {:token :lparen :literal "("}
         {:token :ident :literal "a"}
         {:token :ident :literal "b"}
         {:token :rparen :literal ")"}

         {:token :func :literal "func"}
         {:token :ident :literal "add"}
         {:token :lparen :literal "("}
         {:token :ident :literal "a"}
         {:token :ident :literal "b"}
         {:token :rparen :literal ")"}

         {:token :lbrace :literal "{"}
         {:token :return :literal "return"}
         {:token :ident :literal "a"}
         {:token :plus :literal "+"}
         {:token :ident :literal "b"}
         {:token :semi-colon :literal ";"}
         {:token :rbrace :literal "}"}

         {:token :if :literal "if"}
         {:token :lparen :literal "("}
         {:token :ident :literal "a"}
         {:token :gt :literal ">"}
         {:token :long :literal 10}
         {:token :rparen :literal ")"}

         {:token :lbrace :literal "{"}
         {:token :return :literal "return"}
         {:token :ident :literal "x"}
         {:token :semi-colon :literal ";"}
         {:token :rbrace :literal "}"}

         {:token :else :literal "else"}
         {:token :lbrace :literal "{"}
         {:token :return :literal "return"}
         {:token :ident :literal "add"}
         {:token :lparen :literal "("}
         {:token :ident :literal "a"}
         {:token :comma :literal ","}
         {:token :long :literal 10}
         {:token :rparen :literal ")"}
         {:token :semi-colon :literal ";"}
         {:token :rbrace :literal "}"}])))
