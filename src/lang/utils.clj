(ns lang.utils)

;;just some info around current token
(defn next-info [token-list type]
  (let [found (some #(when (= type (first %)) %) token-list)
        token-wanted #{[type (second found)]}
        point-to-token #(not (token-wanted %))
        before (take-while point-to-token token-list)
        in (drop-while point-to-token token-list)
        after (rest in)
        after-paren-close-tokens (rest (drop-while 
                                        #(not (#{[:paren-close
                                                  ")"]} %))
                                        after))
        between-parens (take-while #(not (#{[:paren-close
                                             ")"]} %))
                                   (rest after))]
    [found token-wanted point-to-token before in
     after after-paren-close-tokens between-parens])) 
(defn next-info2 [token-list how-to-find-func]
  (let [
        found (some how-to-find-func token-list)
        token-wanted #{found}
        before (vec (take-while #(not (token-wanted %)) token-list))
        after (vec (rest (drop-while #(not (token-wanted %)) token-list)))
         prevtoken (last before)
         nexttoken (first after)
         before-operand (vec (drop-last before));;!!to seq conv
         after-operand (vec (rest after))]
    [found token-wanted before after prevtoken 
     nexttoken before-operand after-operand])) 
