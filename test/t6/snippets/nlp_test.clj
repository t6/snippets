(ns t6.snippets.nlp-test
  (:use midje.sweet)
  (:require [t6.snippets.nlp :as nlp]))

^{:refer t6.snippets.nlp/with-db :added "0.1.0"}
(fact
  "Sets the context (bound to `*db*`) of all core.logic relations
  in this namespace."
  (nlp/with-db {:semantic-graphs [:foo]}
    (:semantic-graphs nlp/*db*)) => (just :foo))

^{:refer t6.snippets.nlp/pipeline :added "0.1.0"}
(fact "Creates a pipeline. Before using this function you need to
require the NLP component you want to use. You can then
create a pipeline using e.g. `(pipeline {:type :clearnlp})`,
where `:clearnlp` is your NLP component's name.
You can additionally pass more settings to the NLP component.")

^{:refer t6.snippets.nlp/triple->string :added "0.1.0"}
(fact "triple->string")

^{:refer t6.snippets.nlp/grouped-triple->string :added "0.1.0"}
(fact "grouped-triple->string")

^{:refer t6.snippets.nlp/reified-triple->string :added "0.1.0"}
(fact "reified-triple->string")

^{:refer t6.snippets.nlp/reified-triple->vector :added "0.1.0"}
(fact "Turn a reified triple into a vector `[subject predicate object]`.
This is mostly useful for having a more compact representation of
a triple when debugging at the REPL. The `subject` and `object` values
of the triple are available in the metadata of the appropriate element
in the vector.")

^{:refer t6.snippets.nlp/edge :added "0.1.0"}
(fact "edge")

^{:refer t6.snippets.nlp/depends :added "0.1.0"}
(fact "depends")

^{:refer t6.snippets.nlp/word-map :added "0.1.0"}
(fact "word-map")

^{:refer t6.snippets.nlp/defrelations :no-doc true :added "0.1.0"}
(fact "Defines a new goal for the given typed dependency relations
in terms of `depends`.")

^{:refer t6.snippets.nlp/defcollapsedrelations :no-doc true :added "0.1.0"}
(fact "See `defrelations`.")

^{:refer t6.snippets.nlp/mention-noun-word :added "0.1.0"}
(fact "Returns all noun or pronoun words inside `mention`.")

^{:refer t6.snippets.nlp/singleton-mention? :added "0.1.0"}
(fact "singleton-mention?")

^{:refer t6.snippets.nlp/linked-from-coreferences :added "0.1.0"}
(fact "linked-from-coreferences")

^{:refer t6.snippets.nlp/linked-semantic-graph-node-identity :added "0.1.0"}
(fact "linked-semantic-graph-node-identity")

^{:refer t6.snippets.nlp/linked-wh-words :added "0.1.0"}
(fact "linked-wh-words")

^{:refer t6.snippets.nlp/linked-nn-connected-nouns :added "0.1.0"}
(fact "linked-nn-connected-nouns")

^{:refer t6.snippets.nlp/linked :added "0.1.0"}
(fact "linked")

^{:refer t6.snippets.nlp/linked-word-maps :added "0.1.0"}
(fact "Unifies `q` with a vector of all words `w` is linked to.")

^{:refer t6.snippets.nlp/triple-negation :added "0.1.0"}
(fact "triple-negation")

^{:refer t6.snippets.nlp/triple-query-name :added "0.1.0"}
(fact "triple-query-name")

^{:refer t6.snippets.nlp/triple-queries :added "0.1.0"}
(fact "triple-queries")

^{:refer t6.snippets.nlp/triples :added "0.1.0"}
(fact "triples")

^{:refer t6.snippets.nlp/grouped-triples :added "0.1.0"}
(fact "grouped-triples")

^{:refer t6.snippets.nlp/reify-triple :added "0.1.0"}
(fact "reify-triple")

^{:refer t6.snippets.nlp/triple :added "0.1.0"}
(fact "triple")
