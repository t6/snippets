(ns t6.snippets.nlp.corenlp
  (:refer-clojure :exclude [not chunk char defn])
  (:require [schema.core :as s :refer [defn defschema]]
            [plumbing.core :refer [for-map fnk]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [camel-snake-kebab.core :refer [->kebab-case]]
            [t6.snippets.nlp :as nlp]
            [t6.snippets.util :as u])
  (:import (edu.stanford.nlp.ling CoreAnnotation IndexedWord)
           (edu.stanford.nlp.pipeline StanfordCoreNLP Annotation)
           (edu.stanford.nlp.util TypesafeMap)
           (edu.stanford.nlp.semgraph SemanticGraphEdge)
           (edu.stanford.nlp.hcoref.data CorefChain CorefChain$CorefMention)
           (java.lang.reflect ParameterizedType Method Type)
           (java.net URL)
           (java.util Properties)
           (java.io Writer)))

(defschema Named
  (s/either s/Str s/Symbol s/Keyword))

(defschema PipelineSettings
  {Named (s/either Named s/Bool [Named])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pipeline

(defn load-pipeline-settings :- PipelineSettings
  "load-pipeline-settings"
  {:added "0.1.0"}
  [path :- (s/maybe (s/either s/Str URL))]
  (when path
    (binding [*read-eval* false?]
      (if-let [settings (read-string (slurp (io/reader path)))]
        settings
        (throw (ex-info "CoreNLP pipeline settings could not be loaded"
                        {:path path}))))))

(defn pipeline :- StanfordCoreNLP
  "Returns a new StanfordCoreNLP object with the given settings."
  {:added "0.1.0"}
  ([] (pipeline nil))
  ([settings :- (s/maybe (s/either s/Str PipelineSettings))]
     (let [properties (Properties.)]
       (doseq [[k v] (merge
                      (load-pipeline-settings
                       (io/resource "t6/snippets/corenlp/pipeline.edn"))
                      (cond
                       (string? settings)
                       (load-pipeline-settings settings)

                       (or (nil? settings)
                           (map? settings))
                       settings

                       :else
                       (throw (ex-info "" {}))))]
         (.put properties
               (name k)
               (if (coll? v)
                 (str/join "," (map name v))
                 v)))
       (StanfordCoreNLP. properties))))

(defn annotate :- TypesafeMap
  [pipeline :- StanfordCoreNLP, text :- s/Str]
  (let [document (Annotation. ^String text)]
    (.annotate ^StanfordCoreNLP pipeline ^Annotation document)
    document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotations

;; The following declare form is generated with
(defn gened-publics
  {:private true, :no-doc true}
  []
  (print "(declare ")
  (doseq [fs (->> (ns-publics *ns*)
                  (filter (comp ::gen meta second))
                  (map first)
                  sort
                  (partition 4))
          :let [_ (print "\n  ")]
          f fs]
    (print (str f " ")))
  (print "\n)"))
;; and is here to help the static analyzer of Cursive Clojure to find all
;; generated functions in this namespace.
(declare
  abbr abgene abstr after
  all-relation-mentions annotated-tree answer answer-object
  antecedent arg-descendent argument author
  bag-of-words basic-dependencies be before
  begin-index best-cliques best-full binarized-tree
  calendar candidate-part-of-speech category category-functional-tag
  char character-offset-begin character-offset-end characters
  chinese-char chinese-is-segmented chinese-orig-seg chinese-seg
  chunk class-name co-nll-dep co-nll-dep-parent-index
  co-nll-dep-type co-nll-predicate co-nllsrl coarse-tag
  collapsed-cc-processed-dependencies collapsed-dependencies column-data-classifier common-words
  constraint contexts coref coref-chain
  coref-cluster coref-cluster-id coref-dest coref-graph
  cost-magnification covert-id d-2-l-begin d-2-l-end
  d-2-l-middle day dependency dependents
  dict dist-sim do doc-date
  doc-id doc-source-type doc-title doc-type
  document-directory document-id domain end-index
  entity-class entity-mentions entity-rule entity-type
  event-mentions features female-gaz first-child
  forced-sentence-end forced-sentence-until-end freq gaz
  gazetteer gender generic-tokens genia
  gold-answer gold-class governor grandparent
  have head-tag-label head-word-label head-word-string
  height id idf in
  index interpretation is-date-range is-url
  l-begin l-end l-middle label
  label-id label-weight last-gaz last-tagged
  left-children-node left-term lemma length
  line-number link location male-gaz
  marking mention-token mentions month
  morpho-case morpho-gen morpho-num morpho-pers
  named-entity-tag neighbors nerid node-vector
  normalized-named-entity-tag not num-txt-sentences numeric-composite-object
  numeric-composite-type numeric-composite-value numeric-object numeric-type
  numeric-value numerized-tokens original-char original-text
  para-position paragraph paragraphs parent
  part-of-speech percent phrase-words phrase-words-tag
  polarity position possible-answers predicted-answer
  predicted-class prediction-error predictions prev-child
  prior proto quotations relation-mentions
  rewritten-arabic role section section-date
  section-end section-id section-start semantic-head-tag
  semantic-head-word semantic-tag semantic-word sentence-id
  sentence-index sentence-position sentences shape
  space-before span speaker srl-instances
  srlid stacked-named-entity-tag state stem
  subcategorization tag-label text token-begin
  token-end tokens topic tree
  trigger true-case true-case-text true-tag
  u-block u-type unary unknown
  use-marked-discourse utterance value verb-sense
  web word-form word-position word-sense
  wordnet-syn xml-context xml-element year)

(defn- get-inner-classes
  [prefix class-name]
  (let [class (Class/forName (name class-name))]
    (for-map [^Class inner-class (.getDeclaredClasses class)
              :when (some #{CoreAnnotation} (supers inner-class))
              :let [full-name (.getName inner-class)
                    name (if (.endsWith full-name "Annotation")
                           (subs full-name
                                 (inc (count (.getName class)))
                                 (- (count full-name) (count "Annotation")))
                           (subs full-name (inc (count (.getName class)))))]]
      inner-class
      (symbol (str (.getName *ns*))
              (str prefix (->kebab-case name))))))

(defmacro def-annotation-accessors
  "Generates an accessor function for each `CoreAnnotation` inner class found
  in each of the `annotation-classes`.

  Use them to replace calls like

    (.get document CoreAnnotations$BeginIndexAnnotation)

  with

    (begin-index document)

  Note that the class name `CoreAnnotations$BeginIndexAnnotation` is stripped of
  the suffix `Annotation` and its prefix `CoreAnnotations$` and `BeginIndex` is
  converted to  kebab-case which results in a function name of `begin-index`."
  {:added "0.1.0"}
  [& annotation-classes]
  (let [fns (for [v annotation-classes
                  :let [[prefix annotation-class] (if (vector? v) v [nil v])]
                  [^Class class fn-name] (get-inner-classes prefix annotation-class)]
              `(defn ~fn-name
                 {:doc/format :markdown
                  :doc        ~(format "See [%s](http://nlp.stanford.edu/nlp/javadoc/javanlp/%s.html)"
                                       (.getName class)
                                       (-> (.getName class)
                                           (str/replace #"\." "/")
                                           (str/replace #"\$" ".")))
                  ::gen       true
                  :arglists   '~(list ['annotation])}
                 [annotation#]
                 (.get ^TypesafeMap annotation# ~class)))]
    `(do ~@fns)))

(def-annotation-accessors
  edu.stanford.nlp.ling.CoreAnnotations
  edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations
  edu.stanford.nlp.trees.TreeCoreAnnotations
  edu.stanford.nlp.ie.machinereading.structure.MachineReadingAnnotations
  edu.stanford.nlp.hcoref.CorefCoreAnnotations
  edu.stanford.nlp.international.arabic.process.ArabicDocumentReaderAndWriter
  edu.stanford.nlp.trees.GrammaticalRelation
  edu.stanford.nlp.ling.ChineseCoreAnnotations
  edu.stanford.nlp.parser.common.ParserAnnotations
  edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
  edu.stanford.nlp.sentiment.SentimentCoreAnnotations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sentence maps

(defn sentence-annotation->sentence-map :- nlp/Sentence
  [sentence :- TypesafeMap]
   (let [text  (text sentence)
         begin (character-offset-begin sentence)
         index (sentence-index sentence)
         end   (character-offset-end sentence)]
     (if (and text begin end index)
       {:type  :sentence
        :text  text
        :index index
        :span  [begin end]}
       (throw (ex-info "Sentence has nil values" {:text text
                                                  :index index
                                                  :span [begin end]})))))

(defn sentence-maps :- [nlp/Sentence]
  [document :- TypesafeMap]
  (mapv sentence-annotation->sentence-map
        (sentences ^TypesafeMap document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Token maps

(defn token-annotation->token :- nlp/Token
  [sentence :- TypesafeMap, token :- TypesafeMap]
  (let [word (text token)
        begin (character-offset-begin token)
        end   (character-offset-end token)
        lemma (lemma token)
        index (index token)
        sent  (sentence-index sentence)
        ne    (named-entity-tag token)
        tag   (part-of-speech token)]
    (if (and word begin end lemma ne tag index sent)
      {:type     :token
       :token    word
       :index    index
       :sentence sent
       :span     [begin end]
       :lemma    lemma
       :ne       ne
       :tag      tag}
      (throw (ex-info "Token has nil values" {:word     word
                                              :span     [begin end]
                                              :lemma    lemma
                                              :ne       ne
                                              :tag      tag
                                              :sentence sent
                                              :index    index})))))

(defn sentence-annotation->tokens :- [nlp/Token]
  [sentence :- TypesafeMap]
  (mapv (partial token-annotation->token sentence)
        (tokens sentence)))

(defn token-maps :- [[nlp/Token]]
  [document :- TypesafeMap]
  (mapv sentence-annotation->tokens
        (sentences document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Semantic graph

(defn indexed-word->word-map :- nlp/Word
  [^IndexedWord iw :- IndexedWord]
  (let [sentence (.sentIndex iw)
        token    (.word iw)
        lemma    (.lemma iw)
        tag      (.tag iw)
        span     [(.beginPosition iw) (.endPosition iw)]
        index    (.index iw)]
    (if (and sentence token lemma tag index)
      {:type     :word
       :sentence sentence
       :token    token
       :lemma    lemma
       :tag      tag
       :span     span
       :index    index}
      (throw (ex-info "IndexedWord with nil values" {:indexed-word iw})))))

(defn semantic-graph-edge-iter
  [acc ^SemanticGraphEdge e]
  (let [dep (.getDependent e)
        gov (.getGovernor e)
        rel (.getRelation e)
        [reln reln-cc] (str/split (str rel) #"_" 2)]
    (if (and dep gov reln)
      (let [governor (indexed-word->word-map gov)]
        (assoc acc
          governor
          (conj (get acc governor #{})
                [(if reln-cc
                   [(keyword reln) (keyword reln-cc)]
                   (keyword reln))
                 (indexed-word->word-map dep)])))
      (throw (ex-info "SemanticGraphEdge with nil values" {:edge e})))))

(defn semantic-graph-edges
  [graph]
  (reduce semantic-graph-edge-iter
          {}
          (.edgeListSorted ^edu.stanford.nlp.semgraph.SemanticGraph graph)))

(defn semantic-graph-nodes
  [graph]
  (set (map indexed-word->word-map
            (.vertexListSorted ^edu.stanford.nlp.semgraph.SemanticGraph graph))))

(defn sentence-annotation->semantic-graph :- nlp/SemanticGraph
  [^TypesafeMap sentence :- TypesafeMap]
  (let [^edu.stanford.nlp.semgraph.SemanticGraph
        graph (collapsed-cc-processed-dependencies sentence)
        index (sentence-index sentence)
        begin (character-offset-begin sentence)
        end   (character-offset-end sentence)]
    (if (and graph begin index end)
      {:type  :semantic-graph
       :edges (semantic-graph-edges graph)
       :nodes (semantic-graph-nodes graph)
       :index index
       :span  [begin end]}
      (throw (ex-info (format "No semantic graph for sentence" {:sentence sentence}))))))

(defn semantic-graphs
  [document :- TypesafeMap]
  (mapv sentence-annotation->semantic-graph
        (sentences ^TypesafeMap document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coreference resolution

(defmacro throw-unless
  "throw-unless"
  {:added "0.1.0"}
  [expr msg]
  `(if-let [v# ~expr]
     v#
     (throw (ex-info ~msg {}))))

(defn mention->map :- nlp/MentionMap
  [^CorefChain$CorefMention mention :- CorefChain$CorefMention]
  (let [gender      (-> mention .gender u/enum->keyword)
        animacy     (-> mention .animacy u/enum->keyword)
        cluster-id  (.corefClusterID mention)
        id          (.mentionID mention)
        type        (-> mention .mentionType u/enum->keyword)
        text        (.mentionSpan mention)
        number      (-> mention .number u/enum->keyword)
        sentence    (.sentNum mention)
        start-index (.startIndex mention)
        end-index   (.endIndex mention)]
    (if (and gender animacy cluster-id id text type start-index end-index number sentence)
      (let [;; adjust indices and sentence number to match those
            ;; extracted from CoreNLP's semantic graph!
            span     [start-index end-index]
            sentence (dec sentence)]
        (assert (>= sentence 0))
        {:type         :mention-map
         :cluster      cluster-id
         :id           id
         :text         text
         :sentence     sentence
         :span         span
         :gender       (throw-unless
                        (#{:male :female :neutral :unknown} gender)
                        (str "Unknown gender: " gender))
         :animacy      (throw-unless
                        (#{:animate :inanimate :unknown} animacy)
                        (str "Unknown animacy: " animacy))
         :mention-type (throw-unless
                        (#{:list :pronominal :nominal :proper} type)
                        (str "Unknown mention type: " type))
         :number       (throw-unless
                        (#{:singular :plural :unknown} number)
                        (str "Unknown number: " number))})
      (throw (ex-info "CorefMention with nil values" {:mention mention})))))

(defn coreferences
  [document :- TypesafeMap]
  (for-map [[id chain] (coref-chain ^TypesafeMap document)]
    id
    (mapv mention->map (.getMentionsInTextualOrder ^CorefChain chain))))

(defmethod nlp/pipeline :corenlp
  ([settings]
     (let [pipeline (pipeline settings)]
       (fn [text]
         (let [annotation (annotate pipeline text)]
           (reify
             nlp/IAnnotation

             (sentences [_]
               (sentence-maps annotation))

             (tokens [_]
               (token-maps annotation))

             (semantic-graphs [_]
               (semantic-graphs annotation))

             (coreferences [_]
               (coreferences annotation))))))))
