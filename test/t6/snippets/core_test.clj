(ns t6.snippets.core-test
  (:use midje.sweet)
  (:require [t6.snippets.core :refer :all]
            [t6.snippets.nlp :as nlp]
            t6.snippets.nlp.clearnlp))

^{:refer t6.snippets.core/create
  :added "0.1.0"
  :arglists (quote ([{:keys [text pipeline queries]}]))}
(fact "Create a new lazy NLP model, that uses the given map as basis."
      (create {:pipeline {:type :clearnlp :text "This is a test."}})
  #_(let [model (create {:pipeline {:type :clearnlp}
                       :text "This is a test."})]
    (set (keys model)) => (contains #{:text :sentences :semantic-graphs :tokens
                                      :coreferences :triples :grouped-triples
                                      :reified-triples})
    (-> model :sentences) => (just (contains {:text "This is a test."
                                              :span [0 15]}))))
