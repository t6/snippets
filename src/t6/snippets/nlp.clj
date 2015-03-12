(ns t6.snippets.nlp
  (:refer-clojure :exclude [conj ref mod num comp agent defn])
  (:require [schema.core :as s :refer (defn defschema)]
            [plumbing.core :refer (defnk fnk letk for-map)]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.logic :as l]
            [clojure.walk :refer (postwalk)]
            [clojure.tools.macro :refer (name-with-attributes)]
            [schema.core :as s]
            [t6.snippets.span :as span]
            [t6.snippets.util :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NLP data structures

(defschema Sentence
  "Represents a sentence in the input text.

    :text  a substring of the input text as implied by the sentence's :span
    :index the position of the sentence in the text

  "
  {:type (s/enum :sentence)
   :text s/Str
   :index s/Int
   :span span/Span})

(defschema Token
  {:type (s/enum :token)
   :sentence s/Int
   :index s/Int
   :token s/Str
   :lemma s/Str
   :tag s/Str
   :span span/Span
   (s/optional-key :ne) s/Str})

(defschema Word
  "While this may seem the same as a Token it is not.
  A word map corresponds to a node in the semantic graph.
  Semantic graphs do not guarantee that every token
  of the input text is mapped to one of its nodes."
  {:type     (s/enum :word)
   :sentence s/Int
   :index    s/Int
   :token    s/Str
   :lemma    s/Str
   :tag      s/Str
   :span     span/Span})

(defschema WordRelation
  "The label of an edge in a semantic graph. Some NLP libraries
  post-processes the graph and collapse some nodes into one.
  CoreNLP collapses among others conjunctions and the resulting
  relation may be e.g. named \"conj_and\" or \"conj_or\".
  We represent this as a vector [:conj :and] or [:conj :or] instead."
  (s/either s/Keyword [(s/one s/Keyword "") (s/one s/Keyword "")]))

(defschema AdjacentNode
  "An adjacent node is a vector of the edge label to the node
  and the node itself."
  [(s/one WordRelation "edge label")
   (s/one Word "node")])

(defschema SemanticGraph
  "A Clojure representation of a semantic (or dependency) graph.
  The graph is given as a vector of nodes (-> :nodes) with an adjacency
  function that maps the nodes to their adjacent nodes (-> :adjacent).

  :span  The graph corresponds to a sentence in the input text. Thus the
         span of a semantic graph refers to a substring of the input text.
  :index The position of the graph's sentence in the text
  "
  {:type  (s/enum :semantic-graph)
   :edges {Word #{AdjacentNode}}
   :nodes #{Word}
   :index s/Int
   :span  span/Span})

(defschema MentionMap
  ""
  {:type         (s/enum :mention-map)
   :cluster      s/Int
   :text         s/Str
   :sentence     s/Int
   ;; this is a span of token indices, not a text range
   :span         span/Span
   :mention-type (s/enum :list :nominal :pronominal :proper)
   :animacy      (s/enum :animate :inanimate :unknown)
   :gender       (s/enum :male :neutral :unknown)
   :number       (s/enum :plural :singular :unknown)
   (s/optional-key :id) s/Int})

(defschema CorefChainMap
  {s/Int [MentionMap]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Triples

(defschema PredicatePrimitive
  {:type      (s/enum :predicate-primitive)
   :derived?  s/Bool
   :word      (s/either s/Keyword Word)
   :negation  (s/maybe Word)})

(defschema TriplePrimitive
  "A triple is made of a single word `subject` and `object` and a `predicate`
  that performs some action with the `subject` or `object`. The `predicate` can
  either be a corresponding word map or a keyword if the action is implied by
  the semantic graph."
  {:type      (s/enum :triple-primitive)
   :subject   Word
   :predicate PredicatePrimitive
   :object    Word
   :query     s/Keyword})

(defschema GroupedTriplePrimitive
  "A GroupedTriple represents a triple where the subject (or `subject-group`)
  and object (or `object-group`) is made up of all words in the original text
  that are together in a coref chain.

  It preserves the original triple it is based on its metadata (`:origin`)."
  {:type          (s/enum :grouped-triple-primitive)
   :subject-group [Word]
   :predicate     PredicatePrimitive
   :object-group  [Word]})

(defschema WordGroup
  "A `ReifiedTriple` is based on a `GroupedTriple`. It associates a unique
  symbol with a subject or object group. `subject` and `object` are a map with
  keys `:symbol`, `:group`.

  Like a `GroupedTriple` it preserves the `Triple` it is based on (`origin`) in
  its metadata, so that the triple's identity only depends on its `subject`,
  `object` and `predicate`."
  {:type   (s/enum :word-group)
   :symbol s/Symbol
   :group  [Word]})

(defschema Predicate
  {:type     (s/enum :predicate)
   ;; TODO: Rename to keyword?
   :symbol   s/Keyword
   :negated? s/Bool})

(defschema Triple
  {:type      (s/enum :triple)
   :subject   WordGroup
   :predicate Predicate
   :object    WordGroup})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating and working with NLP pipelines

(defschema db-schema
  {(s/optional-key :coreferences)    CorefChainMap
   (s/optional-key :semantic-graphs) [SemanticGraph]
   (s/optional-key :triples)         [Triple]
   (s/optional-key :queries)         (s/either clojure.lang.IDeref [s/Any])})

(s/def ^:dynamic *db* {})

(defmacro with-db
  "Sets the context (bound to `*db*`) of all core.logic relations
  in this namespace.

  (nlp/with-db {:semantic-graphs [:foo]}
    (:semantic-graphs nlp/*db*)) => (just :foo)"
  {:added "0.1.0"}
  [db & body]
  `(binding [*db* ~db]
     (doall ~@body)))

(comment
  sentences       [IAnnotation -> [Sentence]]
  tokens          [IAnnotation -> [Token]]
  semantic-graphs [IAnnotation -> [SemanticGraph]]
  coreferences    [IAnnotation -> CorefChainMap])

(defprotocol IAnnotation
  (sentences [this]
    "Returns all sentences in the annotated text.
    The sentences are sorted in the same order as
    they appeared in the text.")

  (tokens [this]
    "Returns all sentences in the annotated text. A sentence is
    represented as a vector of tokens. The sentences and tokens
    are sorted in the order the appear in the text.")

  (semantic-graphs [this]
    "Returns the semantic graphs of the annotated text. Each
    sentence has one associated semantic graph, showing the
    dependencies between sentence parts. The graphs (sentences)
    are sorted in the same order as they appeared in the text
    (Sentence `i` has a graph at index `i `).")

  (coreferences [this]
    "Returns the coreferences found in the annotated text.
    Sentence parts are coreferent when the refer to the same
    entity in the text."))

(defmulti pipeline
  "Creates a pipeline. A pipeline is a function `[String -> IAnnotation]`
  that annotates the given text. Before using this function you need to
  require the NLP component you want to use. You can then create a pipeline
  using e.g. `(pipeline {:type :clearnlp})`, where `:clearnlp` is your NLP
  component's name. You can additionally pass more settings to the NLP component."
  {:added "0.1.0"}
  (fn [settings] (:type settings)))

;; a dummy pipeline that returns empty annotations
(defmethod pipeline :dummy
  [settings]
  (fn [text]
    (reify IAnnotation
      (sentences [_] [])
      (tokens [_] [])
      (semantic-graphs [_] [])
      (coreferences [_] {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structure utility functions

(defn word->text :- s/Str
  [word :- (s/maybe Word)]
  (if word
    (letk [[token tag sentence index] word]
      (format "%s-%s-%s:%s" token (name tag) sentence index))
    "(nil)"))

(defn triple-primitive->string :- s/Str
  "triple->string"
  {:added "0.1.0"}
  ([t :- TriplePrimitive] (triple-primitive->string "[%s %s %s]" t))
  ([fmt :- s/Str
    {:keys [subject predicate object]} :- TriplePrimitive]
    (format fmt
            (word->text subject)
            (str (if (:derived? predicate)
                   (str (:word predicate))
                   (word->text (:word predicate)))
                 (if (:negation predicate)
                   (str "[" (word->text (:negation predicate)) "]")))
            (word->text object))))

(defn grouped-triple-primitive->string :- s/Str
  "grouped-triple->string"
  {:added "0.1.0"}
  ([t :- GroupedTriplePrimitive] (grouped-triple-primitive->string "[%s %s %s]" t))
  ([fmt :- s/Str
    {:keys [subject-group object-group predicate]} :- GroupedTriplePrimitive]
            (format fmt
                    (print-str (mapv word->text subject-group))
                    (str (if (:derived? predicate)
                           (str (:word predicate))
                           (word->text (:word predicate)))
                         (if (:negation predicate)
                           (str "[" (word->text (:negation predicate)) "]")))
                    (print-str (mapv word->text object-group)))))

(defn triple->string :- s/Str
  "triple->string"
  {:added "0.1.0"}
  ([t :- Triple] (triple->string "[%s %s %s]" t))
  ([fmt :- s/Str
    {:keys [subject object predicate]} :- Triple]
     (apply format fmt (mapv :symbol [subject predicate object]))))

(defn triple->vector
  :- [(s/one s/Symbol "subject")
      (s/one s/Keyword "predicate")
      (s/one s/Symbol "object")]
  "Turn a reified triple into a vector `[subject predicate object]`.
This is mostly useful for having a more compact representation of
a triple when debugging at the REPL. The `subject` and `object` values
of the triple are available in the metadata of the appropriate element
in the vector."
  {:added "0.1.0"}
  [{:keys [subject object predicate] :as t} :- Triple]
  (with-meta (mapv :symbol [subject predicate object])
    (meta t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates for working with part of speech tags

(def TagMap
  (s/maybe {:tag s/Str, s/Any s/Any}))

(defn has-tags? :- s/Bool
  [tags :- #{s/Str}
   {:keys [tag]} :- TagMap]
  (boolean (tags tag)))

(defn verb?
  [m]
  (has-tags? #{"VB" "VBD" "VBG" "VBN" "VBP" "VBZ"} m))

(defn pronoun?
  [m]
  (has-tags? #{"PRP" "WP" "PRP$" "WP$"} m))

(defn noun?
  [m]
  (has-tags? #{"NN" "NNS" "NNP" "NNPS"} m))

(defn determiner?
  [m]
  (has-tags? #{"DT"} m))

(defn wh-word?
  [m]
  (has-tags? #{"WDT" "WP" "WP$" "WRB"} m))

(defn plural? :- Boolean
  [{:keys [tag] :as m} :- TagMap]
  (if (string? tag)
    (boolean (and (noun? m)
                  (.endsWith ^String tag "S")))
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `core.logic` relations for working with Words

(defn edge
  "edge"
  {:added "0.1.0"}
  [parent label child]
  (fn [a]
    (l/to-stream
     (for [graph     (:semantic-graphs *db*)
           [p edges] (:edges graph)
           [l c]     edges]
       (l/unify a [parent label child] [p l c])))))

(defn ^:deprecated depends
  [child label parent]
  (edge parent label child))

(defn word
  "word"
  {:added "0.1.0"}
  [node]
  (fn [a]
    (l/to-stream
     (for [graph (:semantic-graphs *db*)
           n     (:nodes graph)]
       (l/unify a node n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience wrappers for accessing the `edge` relation using
;;; something akin to the Stanford Dependencies representation

(defmacro defrelations
  "Defines a new goal for the given typed dependency relations
in terms of `depends`."
  {:no-doc true :added "0.1.0"}
  [& syms]
  `(do
     ~@(for [s syms]
         `(defn ~s
           ~(str "Typed dependency relation `" s "`.")
           [~'governor ~'dependent]
           (edge ~'governor ~(keyword (name s)) ~'dependent)))))

(defmacro defcollapsedrelations
  "See `defrelations`."
  {:no-doc true :added "0.1.0"}
  [& syms]
  `(do
     ~@(for [s syms]
         `(defn ~s
            ~(str "Collapsed typed dependency relation `" s "`.")
            [~'relation ~'governor ~'dependent]
            (edge ~'governor [~(keyword (name s)) ~'relation] ~'dependent)))))

(declare
 dep aux auxpass cop arg agent comp acomp ccomp xcomp obj dobj iobj
 pobj subj nsubj nsubjpass csubj csubjpass cc expl mod amod appos
 advcl det predet preconj vmod mwe mark advmod neg rcmod quantmod
 nn npadvmod tmod num number poss possessive prt parataxis punct
 ref sdep xsubj)

(defrelations
  dep aux auxpass cop arg agent comp acomp ccomp xcomp obj dobj iobj
  pobj subj nsubj nsubjpass csubj csubjpass cc expl mod amod appos
  advcl det predet preconj vmod mwe mark advmod neg rcmod quantmod
  nn npadvmod tmod num number poss possessive prt parataxis punct
  ref sdep xsubj)

(declare
 conj prep)

(defcollapsedrelations
  conj prep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `core.logic` relations for working with coreferences

(defn mention-noun-word
  "Returns all noun or pronoun words inside `mention`."
  {:added "0.1.0"}
  [mention q]
  (l/project [mention]
   (l/fresh [index tag sentence]
     (l/featurec mention {:sentence sentence})
     (word q)
     (l/pred q (some-fn pronoun? noun? determiner?))
     (l/featurec q {:index index, :sentence sentence})
     (l/pred index (partial span/point-inside? mention)))))

(defn singleton-mention? :- Boolean
  "singleton-mention?"
  {:added "0.1.0"}
  [mentions :- [MentionMap]]
  (= 1 (count mentions)))

(defn linked-from-coreferences
  "linked-from-coreferences"
  {:added "0.1.0"}
  [w1 w2]
  (l/fresh [m1 m2 mentions]
    ;; get each (non-singleton) coref cluster in turn
    (l/membero mentions (vals (:coreferences *db*)))
    (l/pred mentions (complement singleton-mention?))

    ;; select mentions from same cluster
    (l/membero m1 mentions)
    (l/membero m2 mentions)

    ;; infer noun words (from semantic graph) from cluster
    ;; if successful we've found our linked nodes
    (mention-noun-word m1 w1)
    (mention-noun-word m2 w2)))

(defn linked-semantic-graph-node-identity
  "linked-semantic-graph-node-identity"
  {:added "0.1.0"}
  [w1 w2]
  ;; add identities for every word (that has a
  ;; corresponding node on a semantic graph)
  (l/fresh [w]
    (word w)
    (l/== [w1 w2] [w w])))

(defn linked-wh-words
  "linked-wh-words"
  {:added "0.1.0"}
  [w1 w2]
  ;; Look for wh-words connected via :nsubj
  (l/fresh [wdt jj subj reln]
    (l/pred wdt wh-word?)
    (edge jj reln wdt)
    (l/membero reln [:nsubj :nsubjpass])
    (edge subj :rcmod jj)
    (l/== [w1 w2] [wdt subj])))

(defn linked-nn-connected-nouns
  "linked-nn-connected-nouns"
  {:added "0.1.0"}
  [w1 w2]
  ;; Look for nouns that are connected via :nn
  ;; they're not always all picked up by the coref chain
  (l/fresh [noun1 noun2]
    (edge noun2 :nn noun1)
    (l/pred noun1 noun?)
    (l/pred noun2 noun?)
    (l/== [w1 w2] [noun1 noun2])))

(defn linked
  "linked"
  {:added "0.1.0"}
  [w1 w2]
  (l/conde
   [(linked-from-coreferences w1 w2)]
   [(linked-semantic-graph-node-identity w1 w2)]
   [(linked-wh-words w1 w2)]
   [(linked-nn-connected-nouns w1 w2)]))

(defn linked-words
  "Unifies `q` with a vector of all words `w` is linked to."
  {:added "0.1.0"}
  [w q]
  (l/project [w]
    (->> (l/run-nc* [q] (linked w q))
        distinct
        (sort-by (juxt :sentence :index))
        vec
        (l/== q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Triple creation

(defn ^:private triple-negation
  [q triple]
  (l/fresh [negation subject predicate object]
    (l/== triple [subject predicate object])
    ;; check if we can find some truth value for our triple
    ;; need to soft cut (`conda`) here because we don't want to end up with
    ;; conflicting triples like `[spoon :be is]` and `[spoon :be[not] be]`
    ;; (this might happen if we would use `conde` and because
    ;; `(== negation nil)` will always succeed)
    (l/conda
     [;; `conde` here because if the semantic graph contains more than one
      ;; potential negation we want to have a triple for all of them
      (l/conde
       ;; check if predicate has a "neg" relation with some other word
       [(neg predicate negation)
        (l/pred negation #(has-tags? #{"RB"} %1))]

       ;; check if the object has a "neg" relation with some other word
       [(neg object negation)
        (l/pred negation #(has-tags? #{"RB"} %1))]

       ;; look for a verb without its own subject and check if it's negated
       ;; which we interpret to also negate the triple's predicate
       [(l/fresh [verb]
          (xcomp verb predicate)
          (l/pred verb verb?)
          (neg verb negation)
          (l/pred negation #(has-tags? #{"RB"} %1)))]

       ;; if the predicate has no corresponding word in the semantic graph
       ;; check if there is a determiner attached and if that determiner
       ;; is "no"
       ;; TODO: there are probably more determiners than "no" here
       [(l/pred predicate keyword?)
        (det subject negation)
        (l/featurec negation {:tag :DT :lemma "no"})])]

     ;; no negation!
     [(l/== negation nil)])
    (l/== q [subject predicate object negation])))

(defn ^:private triple-query-name
  [v]
  (let [{:keys [ns name]} (meta v)]
    (if (and ns name)
      (symbol (str (.getName ns)) (str name))
      (throw (ex-info "No name for triple query"
                      {:query v})))))

(defn ^:private triple-queries
  [q]
  (l/fresh [triple subject predicate object negation query query-name]
    (l/membero query (:queries *db* []))
    (l/project [query]
      (l/== query-name (triple-query-name query))
      (query triple))

    (triple-negation [subject predicate object negation] triple)

    ;; prevent triples where subject and object refer to the same word map
    (l/!= subject object)

    (l/project [subject predicate object negation query-name]
      (l/== q {:type :triple-primitive
               :subject subject
               :predicate {:derived? (keyword? predicate)
                           :type :predicate-primitive
                           :word predicate
                           :negation negation}
               :object object
               :query query-name}))))

(defn triple-primitives :- [TriplePrimitive]
  "triples"
  {:added "0.1.0"}
  []
  (vec (distinct (l/run-nc* [q] (triple-queries q)))))

(defn grouped-triple-primitives :- [GroupedTriplePrimitive]
  "grouped-triples"
  {:added "0.1.0"}
  []
  (vec
   (distinct
    (l/run-nc* [q]
      (l/fresh [triple subject object subject-group object-group]
        (triple-queries triple)
        (l/featurec triple {:subject subject, :object object})
        (linked-words subject subject-group)
        (linked-words object object-group)

        ;; prevent triples where subject and object refer to the same word group
        (l/!= subject-group object-group)

        (l/project [triple subject-group object-group]
                   (l/== q (with-meta
                             {:type          :grouped-triple-primitive
                              :subject-group subject-group
                              :predicate     (:predicate triple)
                              :object-group  object-group}
                             {:origin triple}))))))))

(defn representative-symbol :- s/Symbol
  [words :- [Word]]
  (let [;; to build nice looking symbols, try to only build it out of
        ;; nouns. If this is impossible use pronouns, then all words.
        ws (filter noun? words)
        ws (if (empty? ws)
             (filter pronoun? words)
             ws)
        ws (if (empty? ws)
             words
             ws)
        s  (->> ws
                (map (fnk [lemma] (str/lower-case lemma)))
                sort
                distinct
                (str/join "-"))]
    (symbol (if (Character/isDigit ^Character (nth s 0)) (str "num-" s) s))))

(defnk reify-predicate :- Predicate
  [derived? word negation]
  {:type :predicate
   :symbol (keyword (str (if negation
                           "not-")
                         (if (keyword? word)
                           (name word)
                           (:lemma word))))
   :negated? (boolean negation)})

(defn ^:private reify-triple :- Triple
  [group->unique-symbol :- {[Word] s/Symbol}
   {:keys [subject-group predicate object-group negation] :as t} :- GroupedTriplePrimitive]
  (let [subject-sym (group->unique-symbol subject-group)
        pred        (reify-predicate predicate)
        object-sym  (group->unique-symbol object-group)]
    (if (and subject-sym pred object-sym)
      (with-meta
        {:type      :triple
         :subject   {:type   :word-group
                     :symbol subject-sym
                     :group  subject-group}
         :predicate pred
         :object    {:type   :word-group
                     :symbol object-sym
                     :group  object-group}}
        (meta t))
      (throw (ex-info (format "Couldn't find symbol: [%s %s %s]"
                              subject-sym pred object-sym)
                      {:grouped-triple t
                       :group->unique-symbol group->unique-symbol})))))

(defn ^:private symbol->groups :- {s/Symbol #{[Word]}}
  [grouped-triples :- [GroupedTriplePrimitive]]
  (->> grouped-triples
       ;; build a sequence of maps that map a symbol to the set of all
       ;; noun groups that have that symbol
       (map (fnk [subject-group predicate object-group]
              ;; This needs to be merged because the subject's and
              ;; object's symbol might be the same while subject !=
              ;; object (they might be in different noun groups) if we
              ;; would not merge here we would potentially loose noun
              ;; groups
              (merge-with set/union
                          {(representative-symbol subject-group) #{subject-group}}
                          {(representative-symbol object-group) #{object-group}})))
       ;; merge everything into one big map while taking care not to
       ;; loose some noun groups while doing so
       (reduce (partial merge-with set/union) {})))

(defn ^:private group->unique-symbol
  [grouped-triples]
  (for-map [[sym groups] (symbol->groups grouped-triples)
            [i group] (map-indexed vector groups)]
    group (symbol (str sym "-" i))))

(defn reify-triples :- [Triple]
  [grouped-triples :- [GroupedTriplePrimitive]]
  (->> grouped-triples
       (map (partial reify-triple (group->unique-symbol grouped-triples)))
       distinct
       vec))

(defn triple
  "triple"
  {:added "0.1.0"}
  [t]
  (fn [a]
    (l/to-stream
     (for [triple (:triples *db*)]
       (l/unify a t triple)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Triple queries definition

(def triple-query-registry
  (atom {}))

(defn register-triple-query!
  [query]
  (when (var? query)
    (add-watch triple-query-registry
               ::dispatch-watchers
               (fn [_ _ _ _]
                 (doseq [[key watcher] (@triple-query-registry query)]
                   (watcher key query)))))
  (swap! triple-query-registry update-in [query] merge {}))

(defn unregister-triple-query!
  [query]
  (when (var? query)
    (remove-watch query ::dispatch-watchers))
  (swap! triple-query-registry clojure.core/disj query))

(defn watch-query
  "Calls `watch-fn` when the query changes. Note that query has to be a var."
  [query key watch-fn]
  (swap! triple-query-registry
         (fn [registry]
           (if-let [_ (registry query)]
             (update-in registry [query] merge {key watch-fn})
             registry))))

(defmacro ^:no-doc =>
  [var triple & templates]
  (if (zero? (count templates))
    (throw
     (Exception.
      "Check your `=>` usages. You need to provide at least one triple template!"))
    `(l/conde
      ~@(for [template templates]
          `[(l/== ~triple ~template)
            (l/project [~triple]
                       (do #_(validate-triple ~var ~triple)
                           l/succeed))]))))

(defmacro defquery
  [name & goals]
  (let [[name goals] (name-with-attributes name goals)
        triple       (gensym "triple")
        lvars        (atom {})
        counter      (atom -1)
        name         (vary-meta name assoc
                                :no-check true
                                :arglists ''([triple])
                                :query? true)
        goals        (clojure.walk/postwalk
                      (fn [x]
                        (cond
                         (and (symbol? x)
                              (= (first (str x)) \?))
                         (do
                           (swap! lvars update-in [x] #(inc (or %1 0)))
                           x)

                         (= x '_)
                         `(l/lvar ~(str "underscore" (swap! counter inc)))

                         (and (seq? x)
                              (= '=> (first x)))
                         `(=> (var ~name) ~triple ~@(rest x))

                         :else
                         x))
                      goals)]
    (doseq [[sym n] @lvars]
      (condp = n
        1 (println
           (format "WARNING: Variable `%s` is used only once. Consider replacing `%s` with `_`?"
                   sym sym))
        nil))

    `(do (def ~name
           (with-meta
             (fn [~triple]
               (l/fresh [~@(keys (deref lvars))]
                 ~@goals))
             {:ns   *ns*
              :name '~name}))
         (register-triple-query! (var ~name))
         (var ~name))))
