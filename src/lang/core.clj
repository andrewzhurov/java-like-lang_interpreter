(ns lang.core
  (:gen-class)
  (:require [lang.tokens :as t]
            [lang.ast :as ast]
            clojure.pprint))

(defn -main
  [& args]
  (println "May I serve you well, sir.")
  (println "To run your full of grace code type in the path to look")
  (loop [in "begin"] 
    (if (#{"quit" "exit" "shut off" "commit a robbery" "I'm pregnant"}
         in)
      (do (println "Why is the life so rough?")
          (println "Anyway, I'm out.")) 
      (do (try 
            
            (let [
                  slurped (slurp in)
                  ast (ast/astnizer slurped)]
              (println in)
              (println "++++++++++++++++++++++++++\nAST:")
              (println (clojure.pprint/pprint ast))
              (println "++++++++++++++++++++++++++\nSome more?")
              )
            (catch java.io.FileNotFoundException e 
              (println "May your path feel well!"))
            (catch Exception e (str "Wow, seems to be wrong"
                                    (.getMessage e))))
          (recur (read-line)))))) 
