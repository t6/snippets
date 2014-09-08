(ns t6.snippets.nlp.clearnlp
  (:refer-clojure :exclude [defn])
  (:require [schema.core :as s :refer (defn defschema)]
	    [plumbing.core :refer (fnk)]
	    [t6.snippets.nlp :as nlp]
	    [clojure.set :as set]
	    [clojure.java.io :as io]
	    [clojure.core.logic :as l]
	    [t6.snippets.span :as span]
	    [t6.graph-transform.core :as diff]
	    [t6.graph-transform.logic :as diffl])
  (:import (java.io InputStream)
	   (com.clearnlp.nlp NLPGetter NLPMode)
	   (com.clearnlp.component AbstractComponent)
	   (com.clearnlp.reader AbstractReader)
	   (opennlp.tools.tokenize TokenizerModel
				   Tokenizer
				   TokenizerME)
	   (opennlp.tools.util Span)
	   (com.clearnlp.pos POSNode)
	   (com.clearnlp.dependency DEPNode DEPTree)
	   (opennlp.tools.sentdetect SentenceModel
				     SentenceDetector
				     SentenceDetectorME)))

(defschema PartialToken
  {:type     (s/enum :partial-token)
   :sentence s/Int
   :index    s/Int
   :token    s/Str
   :span     span/Span})

(defn component*
  "component*"
  {:added "0.1.0"}
  [model-path :- (s/maybe s/Str),
   language  :- (s/maybe s/Str),
   mode :- (s/maybe s/Str)]
  (delay
   (if (and model-path language mode)
     (NLPGetter/getComponent ^String model-path ^String language ^String mode)
     (throw (ex-info "nil parameter"
		     {:model-path model-path
		      :language   language
		      :mode       mode})))))

(defonce component
  (memoize component*))

(defn init-clearnlp
  "init-clearnlp"
  {:added "0.1.0"}
  [pipeline settings]
  (let [retval
	(if-let [tagger (component "general-en"
				   "english"
				   NLPMode/MODE_POS)]
	  (if-let [parser (component "general-en"
				     "english"
				     NLPMode/MODE_DEP)]
	    (if (and tagger parser)
	      (assoc pipeline
		:tagger tagger
		:parser parser)
	      :nil-components)
	    :parser-failed)
	  :tagger-failed)]
    (if (keyword? retval)
      (throw (ex-info "Loading pipeline components failed"
		      {:reason retval, :settings settings}))
      retval)))

(defn init-opennlp
  "init-opennlp"
  {:added "0.1.0"}
  [pipeline settings]
  ;; TODO: Read model path from settings
  (let [sent-model-url (io/resource "en-sent.bin")]
    (with-open [^InputStream sent-model-in
		(or (and sent-model-url (.openStream sent-model-url))
		    (throw (ex-info "Could not open sentence model"
				    {:url      sent-model-url
				     :settings settings})))]
      (let [model             (SentenceModel. sent-model-in)
	    sentence-detector (SentenceDetectorME. model)
	    ;; TODO: Read model path from settings
	    token-model-url (io/resource "en-token.bin")]
	(with-open [^InputStream token-model-in
		    (or (and token-model-url (.openStream token-model-url))
			(throw (ex-info "Could not open token model"
					{:url      token-model-url
					 :settings settings})))]
	  (let [model (TokenizerModel. token-model-in)
		tokenizer (TokenizerME. model)]
	    (assoc pipeline
	      :tokenizer (delay tokenizer)
	      :sentence-detector (delay sentence-detector))))))))

(defn tokens :- [[PartialToken]]
  "tokens"
  {:added "0.1.0"}
  [pipeline sentences :- [nlp/Sentence]]
  (mapv
   (fnk [text index [:span :as sent-span]]
     (let [counter (atom -1)]
       (mapv
	(fn [^Span span]
	  (if-let [token (.getCoveredText span text)]
	    {:type     :partial-token
	     :sentence index
	     :index    (swap! counter inc)
	     :span     (span/project sent-span
				     [(.getStart span) (.getEnd span)])
	     :token    (str token)}
	    (throw (ex-info "nil covered text" {:span span, :text text}))))
	(when-let [tokenizer (:tokenizer pipeline)]
	  (.tokenizePos ^Tokenizer @tokenizer text)))))
   sentences))

(defn sentences :- [nlp/Sentence]
  "sentences"
  {:added "0.1.0"}
  [pipeline text :- s/Str]
  (let [counter (atom -1)]
    (mapv
     (fn [^Span span]
       (if-let [covered-text (.getCoveredText span text)]
	 {:type  :sentence
	  :index (swap! counter inc)
	  :span  [(.getStart span) (.getEnd span)]
	  :text  (str covered-text)}
	 (throw (ex-info "nil covered text"
			 {:span span, :text text}))))
     (when-let [detector (:sentence-detector pipeline)]
       (.sentPosDetect ^SentenceDetector @detector text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ClearNLP does not collapse conjunctions and prepositions like
;;; CoreNLP. The following graph transformations try to approximate
;;; CoreNLP's semantic graph simplifications.

(defn collapse-conjunctions
  [graph]
  (l/run* [q]
    (l/fresh [?cc ?child ?parent]
      (diffl/edge ?parent :conj ?child)
      (diffl/edge ?parent :cc ?cc)
      (l/project
	  [?cc]
	(l/== q {:edges {:+ {?parent [[(keyword (str "conj_" (:lemma ?cc))) ?child]]}
			 :- {?parent [[:conj ?child] [:cc ?cc]]}}
		 :nodes {:- [?cc]}})))))

(defn propagate-conjoined-nsubj
  [graph]
  (l/run* [q]
    (l/fresh [?cc ?child ?parent ?conj-child]
      (diffl/edge ?parent :conj ?conj-child)
      (diffl/edge ?parent :cc ?cc)
      (diffl/edge ?parent :nsubj ?child)
      (l/project
	  [?cc]
	(l/== q {:edges {:+ {?conj-child [[:nsubj ?child]]}
			 :- {?parent [[:conj ?child] [:cc ?cc]]}}
		 :nodes {:- [?cc]}})))))

(defn collapse-prepositions
  [graph]
  (l/run* [q]
    (l/fresh [?gov ?prep ?child ?parent]
      (diffl/edge ?parent :prep ?prep)
      (diffl/edge ?prep :pobj ?child)
      (l/project
	  [?prep]
	(l/== q {:edges {:+ {?parent [[(keyword (str "prep_" (:lemma ?prep))) ?child]]}
			 :- {?parent [[:prep ?prep]]
			     ?prep   [[:pobj ?child]]}}
		 :nodes {:- [?prep]}})))))

(defn remove-punct
  "Removes all punctuation nodes from the graph."
  [graph]
  (l/run* [q]
    (l/fresh [?punct ?parent]
      (diffl/edge ?parent :punct ?punct)
      (l/== q {:edges {:- {?parent [[:punct ?punct]]}}
	       :nodes {:- [?punct]}}))))

(defn dep-node->word-map :- nlp/Word
  "dep-node->word-map"
  {:added "0.1.0"}
  [tokens :- [PartialToken], node :- DEPNode]
   (let [token     (.-form ^POSNode node)
	 index     (.-id ^DEPNode node)
	 ;; TODO: Not a lemma! Include an external lemmatizer...
	 ;; This is also needed for better token maps
	 lemma     (.-lemma ^POSNode node)
	 tag       (.-pos ^POSNode node)
	 token-map (nth tokens (dec index))
	 span      (:span token-map)
	 sentence  (:sentence token-map)]
    (if (and sentence token lemma tag index)
      {:type     :word
       :sentence sentence
       :token    token
       :lemma    lemma
       :tag      tag
       :span     span
       :index    index}
      (throw (ex-info "Node with nil values"
		      {:node node})))))

(defn root-node? :- Boolean
  "root-node?"
  {:added "0.1.0"}
  [node :- (s/maybe DEPNode)]
  (when node
    (= (.-pos ^POSNode node) "_R_")))

(defn- adjacent-nodes-iter
  "adjacent-nodes-iter"
  {:added "0.1.0"}
  [->word-map acc ^DEPNode node]
  (if (not (root-node? node))
    (let [head (.getHead node)]
      (if (and head (not (root-node? head)))
	(let [label (.getLabel node)
	      word  (->word-map head)]
	  (assert label)
	  (assoc acc word
		 (conj (get acc word #{})
		       [(keyword label) (->word-map node)])))
	acc))
    acc))

(defn dep-tree->semantic-graph :- nlp/SemanticGraph
  "dep-tree->semantic-graph"
  {:added "0.1.0"}
  [tokens :- [PartialToken], tree :- DEPTree]
  (let [;; tree is-a java.util.List (WTF?)
	->word-map  (partial dep-node->word-map tokens)
	nodes       (set (mapv ->word-map (remove root-node? tree)))
	adjacent    (reduce (partial adjacent-nodes-iter ->word-map)
			    {} tree)
	first-token (first tokens)
	last-token  (last tokens)]
    (assert (and first-token last-token))
    {:type  :semantic-graph
     :index (:sentence first-token)
     :span  (span/union first-token last-token)
     :edges adjacent
     :nodes nodes}))

(defn semantic-graphs :- [nlp/SemanticGraph]
  "semantic-graphs"
  {:added "0.1.0"}
  [pipeline -sentences :- [nlp/Sentence]]
  (let [{:keys [tagger parser]} pipeline]
    (mapv
     (fn [tokens]
       (if-let [tree (NLPGetter/toDEPTree (mapv :token tokens))]
	 (do
	   (doseq [component [tagger parser]]
	     (if-not component
	       (throw (ex-info "nil AbstractComponent in pipeline"
			       {:pipeline pipeline})))
	     (.process ^AbstractComponent @component tree))
	   (-> (dep-tree->semantic-graph tokens tree)
	       (diff/transform [collapse-prepositions
				collapse-conjunctions
				propagate-conjoined-nsubj
				remove-punct])))
	 (throw (ex-info "DEPTree was nil" {:tokens tokens}))))
     (tokens pipeline -sentences))))

(defn pipeline
  "pipeline"
  {:added "0.1.0"}
  [settings]
  (-> {}
      (init-clearnlp settings)
      (init-opennlp settings)))

(defmethod clojure.core/print-method DEPNode
  [node ^java.io.Writer w]
  ;; DEPNode instances are horrible to use from the REPL...
  ;; e.g. its native toString method throws NullPointerExceptions
  ;; so we provide this alternative here to prevent that.
  (.write w (str "#<DEPNode " (.form node) "-" (.id node) ">")))

(defmethod nlp/pipeline :clearnlp
  [settings]
  (let [pipeline (pipeline settings)]
    (fn [text]
      (let [-sentences (delay (sentences pipeline text))]
	(reify
	  nlp/IAnnotation

	  (tokens [this]
	    (tokens pipeline @-sentences))

	  (sentences [this]
	    @-sentences)

	  (coreferences [this]
	    {})

	  (semantic-graphs [this]
	    (semantic-graphs pipeline @-sentences)))))))
