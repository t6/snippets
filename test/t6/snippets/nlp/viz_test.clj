(ns t6.snippets.nlp.viz-test
  (:use midje.sweet)
  (:require [t6.snippets.nlp.viz :refer :all]))

^{:refer t6.snippets.nlp.viz/render-semantic-graph :added "0.1.0"}
(fact "render-semantic-graph")
