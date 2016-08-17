(ns lang.ast
  (:require [lang.tokens :as t]
            [lang.utils :refer [next-info next-info2]]))
(def precedence ["*" "/" "+" "-" "==" "!=" "<=" ">=" "<" ">"])
(def pkeywords [:name :if :curl-open :function]) 
(def node-model {:type :t
                 :name "name"
                 :childs []})
;; sample for live testin
(def ts (t/tokenizer-spaceless t/code)) 

;; a little bit of polymorphism
(defmulti node (fn [token-list] (first (first token-list))))
(defmethod node :default ;; for numbers and vars
   [[token]]
  (if (not (map? token))
    (assoc node-model :type (first token) 
           :name (second token))
    token))
;; assumes func call have one already ast-node
(defmethod node :function-call
  [token-list]
  (assoc node-model :type (first (first token-list))
                    :name (second (first token-list))
                    :childs (second (rest token-list))))
(defmethod node :int
  [[int-token [_ name]]]
  (assoc node-model :type :def :name name))
;; assignments
(defmethod node :name
  [[[_ name] ast-node]]
  (assoc node-model :type :let :name name :childs [ast-node]))
(defmethod node :curl-open
  [[_ & ast-list]]
  (assoc node-model :type :do :name "{" :childs (vec ast-list)))
(defmethod node :if
  [[_ expr fir sec]] 
  (assoc node-model :type :if :name "if" :childs [expr fir sec]))
(defmethod node :function
  [[[_ name] args body]]
  (assoc node-model :type :function :name name :childs [args body]))
(defmethod node :last-name
  [[[_ name]]]
  (assoc node-model :type :last-name :name name))



(defn make-ast ;; it can be much better, but I'm tired :)
  ([token-list] (make-ast token-list :function-call))
  ([token-list op]
   (let [[found token-wanted point-to-token
          before in after after-paren-close-tokens 
          between-parens] (next-info token-list 
                                     op)]
     (if found 
       (let [good-in (-> [found [:paren-open "("]
                          (make-ast between-parens)
                          [:paren-close ")"]])]
         (recur (into (conj (vec before) (node good-in))
                      after-paren-close-tokens)
                op))
       (make-ast token-list "*" (rest precedence)))))
  ([token-list op rest-ops] 
   (let [[found token-wanted before 
          after prevtoken nexttoken
          before-operand after-operand] (next-info2
                                         token-list 
                                         #(when (= op (second %)) %))]
     (if found
       (recur (into (conj 
                     before-operand 
                     {:type (first found)
                      :name (second found)
                      :childs [(node [prevtoken])
                               (node [nexttoken])]})
                    after-operand)
              op rest-ops)
       (if (not (empty? rest-ops))
         (recur token-list (first rest-ops) (rest rest-ops))
         token-list)))) ;;changed
  ([token-list keyword rest-kws some]
   (let [[found token-wanted before 
          after prevtoken nexttoken
          before-operand after-operand] (next-info2
                                         token-list 
                                         #(when (= keyword (first %)) %))]
     (if found
       (case keyword
         :name (if (not= nexttoken [:curl-close "}"]) 
                 (-> (conj before (node [found nexttoken]))
                     (into after-operand)
                     (recur :name rest-kws some))
                 (-> (conj before (node [[:last-name (second found)]]))
                     (into after)
                     (recur :name rest-kws some)))
         :if  (let [expr (first after-operand)
                    after-paren (rest (rest (rest after)))
                    [bad-params bad-tail] (split-with #(not= % [:curl-close "}"])
                                              after-paren)
                    params (conj (vec bad-params) [:curl-close "}"])
                    res (vec (rest bad-tail))
                    good-params (make-ast params
                                          :curl-open [] some)
                    fir (first good-params)
                    sec (second good-params)
                  
                    out (->> (node [found expr fir sec])
                             (conj before)
                             (#(into % res)))]
                (recur out :if rest-kws some))
         :function (let [[args after-args] (split-with
                                            #(not= % [:paren-close ")"])
                                            (rest after))
                         args (vec args)
                         after-args (vec after-args)
                         good-args (vec (map #(node [%]) args))
                         ;;good-after (vec (make-ast (rest after-args)))
                         good-after (rest after-args)
                         body (first good-after)
                         rest (rest good-after)
                         fun-node (node [found good-args body])
                         out (into (vec (conj before fun-node))
                                   rest)]
                     (recur out :function rest-kws some)) 
         :curl-open (let [[mychunk before] ;; before with "{",delete it! 
                          (->> token-list
                               reverse
                               (split-with #(not (#{[:curl-open "{"]} %)))
                               (map reverse))
                          [goodchunk after] ;; after with "}"
                          (->> mychunk
                               (split-with #(not (#{[:curl-close "}"]} %))))
                          out (into (conj (vec (drop-last before))
                                          (node (into [[:curl-open "{"]]
                                                      goodchunk)))
                                    (rest after))]
                      (recur out :curl-open rest-kws some)))

       (if (not (empty? rest-kws))
         (recur token-list (first rest-kws) (rest rest-kws) some)
         token-list))))) 

;; demonstrating how the result can be obtained.
(defn astnizer [code]
  (let [tokens (t/tokenizer-spaceless code)
        operators-done (make-ast tokens)
        assignments-done (make-ast operators-done :name [] "some")
        ifs-done (make-ast assignments-done :if [] "some")
        curls-done (make-ast ifs-done :curl-open [] "some")
        funcs-done (make-ast curls-done :function [] "some")]
    funcs-done))
;; we can do this in greately shorter way 
;; (make-ast operators-done [:name :if :curl-open :function] "some")
;; or using default precedence
;; (make-ast operators-done pkeywords "some")
;; I know, I know. "make-ast" bad designed
;; it could be really neat with interface like this:
;; (make-ast tokens pkeywords)
;; let's just imagine we're good with this 
