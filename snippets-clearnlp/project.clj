(defproject t6/snippets-clearnlp "0.1.0-SNAPSHOT"
  :plugins [[lein-modules "0.3.11"]]
  :description "Dependencies for snippets' OpenNLP+ClearNLP pipeline"
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]
  :dependencies [[t6/graph-transform "0.1.0"]
                 [com.clearnlp/clearnlp "2.0.2"]
                 [com.clearnlp/clearnlp-dictionary "1.0"]
                 [com.clearnlp/clearnlp-general-en-pos "1.1"]
                 [com.clearnlp/clearnlp-general-en-dep "1.2"]
                 [com.clearnlp/clearnlp-general-en-srl "1.1"]

                 [org.apache.opennlp/opennlp-tools "1.5.3"]
                 [edu.washington.cs.knowitall/opennlp-tokenize-models "1.5"]
                 [edu.washington.cs.knowitall/opennlp-sent-models "1.5"]])
