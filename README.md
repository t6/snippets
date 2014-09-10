# snippets

An NLP Library for Clojure.

`snippets` is the basis for [clide-nlp](https://github.com/t6/clide-nlp).

## Features

`snippets` implements the core algorithm from my bachelor report [An
NLP Assistant for Clide](http://arxiv.org/abs/1409.2073v1).

The algorithm mines triples in the form `[subject predicate object]`
that are extracted from a text's semantic graphs and linked using
its coreference chain. It uses a series of `core.logic` based queries
to achieve this (see
[`triples.clj`](https://github.com/t6/snippets/blob/master/src/t6/snippets/triples.clj)
for example queries).

Other features include

 * sentence splitting,
 * tokenization,
 * accessing the semantic graphs (aka dependency graphs) of your sentences,
 * and coreference resolution (with CoreNLP backend only).

`snippets` uses Primatic's schema. Please take a look at the schemas
at the top of
[`nlp.clj`](https://github.com/t6/snippets/blob/master/src/t6/snippets/nlp.clj).

## Usage

[![Clojars Project](http://clojars.org/t6/snippets/latest-version.svg)](http://clojars.org/t6/snippets)

`snippets` can currently use two NLP backends. One based on [CoreNLP](http://nlp.stanford.edu/software/corenlp.shtml)
and the other based on [ClearNLP](https://github.com/clearnlp) and [OpenNLP](http://opennlp.apache.org/index.html). Choose a backend and
include one of the following dependencies in your project. Note: The
ClearNLP backend pulls in more than 700 MiB of data.

[![Clojars Project](http://clojars.org/t6/snippets-corenlp/latest-version.svg)](http://clojars.org/t6/snippets-corenlp)

[![Clojars Project](http://clojars.org/t6/snippets-clearnlp/latest-version.svg)](http://clojars.org/t6/snippets-clearnlp)

```clojure
(require '[t6.snippets.core :as snippets]
	 ;; this assumes that you have included `snippets-clearnlp` in your dependencies.
	 't6.snippets.nlp.clearnlp)
(def m (snippets/create {:pipeline {:type :clearnlp}, :text "This is a test."}))
(keys m)
;; => (:semantic-graphs :sentences :tokens :coreferences :reified-triples :triples ...)
(:sentences m)
;; => [{:type :sentence, :index 0, :span [0 15], :text "This is a test."}]
(:triples m)
;; => [{:subject {:type :word, :sentence 0, :token "This", :lemma "this", :tag "DT", :span [0 4], :index 1}, :predicate {:type :word, :sentence 0, :token "is", :lemma "is", :tag "VBZ", :span [5 7], :index 2}, :object {:type :word, :sentence 0, :token "test", :lemma "test", :tag "NN", :span [10 14], :index 4}, :negation nil, :query t6.snippets.triples/nsubj-pred-dobj}]
```

## License

Copyright © 2014 Tobias Kortkamp

Distributed under the GNU Lesser General Public License either version
3 or (at your option) any later version.
