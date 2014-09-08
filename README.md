# snippets

An NLP Library for Clojure.

## Usage

```clojure
(require '[t6.snippets.core :as core]
	 '[t6.snippets.nlp :as nlp]
	 't6.snippets.nlp.clearnlp)
(def m (core/create {:pipeline {:type :clearnlp}, :text "This is a test."}))
(keys m)
;; => (:semantic-graphs :sentences :tokens :coreferences :reified-triples :triples ...)
(:sentences m)
;; => [{:type :sentence, :index 0, :span [0 15], :text "This is a test."}]
(:triples m)
;; => [{:subject {:type :word, :sentence 0, :token "This", :lemma "this", :tag "DT", :span [0 4], :index 1}, :predicate {:type :word, :sentence 0, :token "is", :lemma "is", :tag "VBZ", :span [5 7], :index 2}, :object {:type :word, :sentence 0, :token "test", :lemma "test", :tag "NN", :span [10 14], :index 4}, :negation nil, :query t6.snippets.triples/nsubj-pred-dobj}]
```

## License

Copyright © 2014 Tobias Kortkamp

Distributed under the GNU Lesser General Public License either version 3.0 or (at
your option) any later version.
