(ns t6.snippets.core
  "We define a dependency graph for all computations, so that no result has
  to be computed twice. The graph caches the result of the computations. It
  uses plumbing's lazy graph compiler. Define new steps of the algorithm here."
  (:refer-clojure :exclude [defn])
  (:require [schema.core :as s :refer (defn)]
	    [plumbing.core :refer (fnk defnk letk)]
	    [plumbing.fnk.pfnk :as pfnk]
	    [plumbing.graph :as graph]
	    [clojure.string :as str]
	    [clojure.set :as set]
	    [t6.snippets.nlp :as nlp]
	    [t6.snippets.span :as span]
	    t6.snippets.triples))

(def dependency-graph
  "This describes the library's dependency graph. The parameters to the
  functions all correspond to its dependent keys.

  If you update the graph, do not forget to update the `AnnotationMap`
  type annotation above."
  (graph/graph
   :annotation
   (fnk [pipeline, text :- s/Str]
     (pipeline text))

   :tokens
   (fnk [annotation :- (s/protocol nlp/IAnnotation)]
     (nlp/tokens annotation))

   :sentences
   (fnk [annotation :- (s/protocol nlp/IAnnotation)]
     (nlp/sentences annotation))

   :semantic-graphs
   (fnk [annotation :- (s/protocol nlp/IAnnotation)]
     (nlp/semantic-graphs annotation))

   :coreferences
   (fnk [annotation :- (s/protocol nlp/IAnnotation)]
     (nlp/coreferences annotation))

   :triples
   (fnk [semantic-graphs :- [nlp/SemanticGraph],
	 coreferences ;; :- nlp/CorefChainMap,
	 queries]
     (nlp/with-db {:semantic-graphs semantic-graphs
		   :coreferences    coreferences
		   :queries         queries}
       (nlp/triples)))

   :grouped-triples
   (fnk [semantic-graphs :- [nlp/SemanticGraph],
	 coreferences ;; :- nlp/CorefChainMap,
	 queries]
     (nlp/with-db {:semantic-graphs semantic-graphs
		   :coreferences    coreferences
		   :queries         queries}
       (nlp/grouped-triples)))

   :reified-triples
   (fnk [grouped-triples :- [nlp/GroupedTriple]]
     (nlp/reify-triples grouped-triples))))

(defn queries-from-namespace
  [ns :- s/Symbol]
  (->> (ns-publics ns)
       vals
       (filter (comp :query? meta))))

(defn create
  "Create a new lazy NLP model based on the given map.

  (let [model (create {:pipeline (nlp/pipeline {:type :clearnlp})
		       :text \"This is a test.\"})]
    (set (keys model)) => (contains #{:text :sentences :semantic-graphs :tokens
				      :coreferences :triples :grouped-triples
				      :reified-triples})
    (-> model :sentences) => (just (contains {:text \"This is a test.\"
					      :span [0 15]})))"
  {:added "0.1.0"}
  ([m] (create dependency-graph m))
  ([graph m]
     (letk [[pipeline {text ""}
             {queries (keys @nlp/triple-query-registry)}] m]
       (let [m (merge m {:pipeline (if (map? pipeline)
                                     (nlp/pipeline pipeline)
                                     pipeline)
                         :text (s/validate s/Str text)
                         :queries queries})]
         (merge m ((graph/lazy-compile graph) m))))))
