(ns t6.snippets.nlp.clearnlp-test
  (:use midje.sweet)
  (:require [t6.snippets.nlp.clearnlp :refer :all]))

^{:refer t6.snippets.nlp.clearnlp/component* :added "0.1.0"}
(fact "component*")

^{:refer t6.snippets.nlp.clearnlp/init-clearnlp :added "0.1.0"}
(fact "init-clearnlp")

^{:refer t6.snippets.nlp.clearnlp/init-opennlp :added "0.1.0"}
(fact "init-opennlp")

^{:refer t6.snippets.nlp.clearnlp/tokens :added "0.1.0"}
(fact "tokens")

^{:refer t6.snippets.nlp.clearnlp/sentences :added "0.1.0"}
(fact "sentences")

^{:refer t6.snippets.nlp.clearnlp/dep-node->word-map :added "0.1.0"}
(fact "dep-node->word-map")

^{:refer t6.snippets.nlp.clearnlp/root-node? :added "0.1.0"}
(fact "root-node?")

^{:refer t6.snippets.nlp.clearnlp/adjacent-nodes-iter :added "0.1.0"}
(fact "adjacent-nodes-iter")

^{:refer t6.snippets.nlp.clearnlp/dep-tree->semantic-graph :added "0.1.0"}
(fact "dep-tree->semantic-graph")

^{:refer t6.snippets.nlp.clearnlp/semantic-graphs :added "0.1.0"}
(fact "semantic-graphs")

^{:refer t6.snippets.nlp.clearnlp/pipeline :added "0.1.0"}
(fact "pipeline")
