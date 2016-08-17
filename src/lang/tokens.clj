(ns lang.tokens
  (:require [clojure.test :refer [is testing deftest run-tests]]))

;; rules of syntax
;; a lot of things have been removed, if it's a matter
;; of habbit, we still can use them anyway
(def tmap 
  (array-map #"^[ \n\t]+" :whitespace
             #"^if(?= +)" :if
             #"^return(?= +)" :whitespace ;;
             #"^else(?= +)" :whitespace ;;
             #"^int[ \n\t]*\w+" :whitespace ;; since only integers allawed
             #"^[\w]+(?=\s*\((\w|\s)*\)\s*\{)" :function
             #"^[\w][\w\-]*(?=\s*\()" :function-call
             #"^[a-zA-Z]+(?=[\w\s]*\)[\s]*\{)" :function-arg
             #"^[a-zA-Z]+" :name
             #"^(\*|\/|\=\=|\!\=|\<\=|\>\=|\<|\>|\-|\+)" :operator
             #"^\d+" :number
             #"^\=" :whitespace ;; 
             #"^\;" :whitespace ;; Do we need this? Nah!
             #"^\{" :curl-open
             #"^\}" :curl-close
             #"^\(" :paren-open
             #"^\)" :paren-close))

;; ellegant tokenizer
;; (it'd be supa ellegant if not troubles with regex)
(defn tokenizer
  ([code] (tokenizer code []))
  ([code tokens]
   (if (not-empty code) 
     (let [token (some (fn [[re type]]
                         (let [name (re-find re code)]
                           (if (sequential? name)
                             (let [name (first name)] ;; :( re bag
                               (when name [type name]))
                             (when name [type name]))))
                       tmap)
           used-length (count (second token))
           rest-code (subs code used-length)
           new-tokens (conj tokens token)]
       (if (not (nil? token))
         (recur rest-code new-tokens)
         (throw
          (ex-info "I can't read your code :("
                   {:rest-code rest-code
                    :ready-tokens tokens
                    :new-token token}))))
     tokens)))

;; whitespaceless
(defn tokenizer-spaceless [code]
  (let [tokens (tokenizer code)]
    (filter (fn [[type name]] (when (not= type :whitespace)
                                true))
            tokens)))

;; sample 
(def code "fac (n b k lm) {
	if (n == 0)
		return 11/1;
	else {
		int res;
		res = n * fac(n - 1);
		return res;
	}
}
           fac2 (k b l v){ k + b + l * v }") 


(deftest tokenizer-test
  (testing "simple function"
    (is (= (tokenizer "myfun(){}")
           [[:function "myfun"]
            [:paren-open "("]
            [:paren-close ")"]
            [:curl-open "{"]
            [:curl-close "}"]])))
  (testing "function call"
    (is (= (tokenizer "5 + my-supa-fun(4)")
           [[:number "5"]
            [:whitespace " "]
            [:operator "+"]
            [:whitespace " "]
            [:function "my-supa-fun"]
            [:paren-open "("]
            [:number "4"]
            [:paren-close ")"]]))))

#_(run-tests)
