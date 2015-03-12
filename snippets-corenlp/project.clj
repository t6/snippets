(defproject t6/snippets-corenlp "0.1.0-SNAPSHOT"
  :plugins [[lein-modules "0.3.11"]]
  :description "Dependencies for snippets' CoreNLP pipeline"
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[edu.stanford.nlp/stanford-corenlp "3.5.1"]
                 [edu.stanford.nlp/stanford-corenlp "3.5.1"
                  :classifier "models"]])
