(ns t6.snippets.triples
  (:require [t6.snippets.nlp :as nlp :refer [defquery =>]]
            [clojure.core.logic :as l]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Triple queries

(defquery nsubj-amod
  ;; give some example texts and the triples that should be created
  ;; from them to create automatic tests for the triple builder
  ;; this is attached to the metadata of the triple builder
  ;; throw exception if there is no example defined!
  {:examples [{:text "The hungry cat eats."
               :triples '[[cat :be hungry]]}]}
  ;; triple builders take one implicit parameter

  ;; lvars are introduced implicitly
  ;; should throw exception if lvar is used only once (or never)
  (l/pred ?subj nlp/noun?)
  (nlp/amod ?subj ?adj)

  ;; triple creation using =>
  ;; can include more than one triple
  ;; compiles down to == with implicit parameter
  ;; for each clause
  (=> [?subj :be ?adj]))

(defquery nsubj-VB
  {:examples [{:text "The cat eats."
               :triples '[[cat :do eats]]}
              {:text "The cat has some food."
               :triples []}
              {:text "The cat is."
               :triples []}]}
  (l/pred ?vb #(nlp/has-tags? #{"VBZ" "VBD" "VBP" "VBG"} %1))
  (l/pred ?subj nlp/noun?)
  (nlp/nsubj ?vb ?subj)

  (l/featurec ?vb {:lemma ?lemma})

  (l/conda
   ;; Exclude verbs be and have
   [(l/membero ?lemma ["have" "be"])
    l/fail]
   [(=> [?subj :do ?vb])]))

(defquery wp-question
  {:examples [{:text "What is his birthday?"
               :triples '[[What is birthday]]}
              {:text "Who is this?"
               :triples '[[Who is this]]}]}
  (nlp/nsubj ?what ?birthday)
  (l/pred ?what #(nlp/has-tags? #{"WP"} %1))
  (nlp/cop ?what ?cop)
  (=> [?what ?cop ?birthday]))

(defquery wrb-question
  {:examples [{:text "How are you?"
               :triples '[[How are you]]}]}
  (nlp/advmod ?p ?question-word)
  (nlp/nsubj ?p ?subj)
  (l/pred ?question-word #(nlp/has-tags? #{"WRB"} %1))
  (=> [?question-word ?p ?subj]))

(defquery nsubj-adj-cop
  {:examples [{:text "The cat is often full."
               :triples '[[cat is full]]}]}
  (nlp/nsubj ?adj ?subject)
  (nlp/cop ?adj ?c)
  (=> [?subject ?c ?adj]))

(defquery nsubj-pred-dobj
  "Find nominal subjects connected with direct objects via a verb."
  {:examples [{:text "It eats the mouse."
               :triples '[[It eats mouse]]}]}
  (nlp/nsubj ?activity ?subject)
  (nlp/dobj ?activity ?object)
  (l/conde
   [(=> [?subject ?activity ?object])]
   [(nlp/conj :and ?subject ?subject2)
    (=> [?subject2 ?activity ?object])]))

(defquery nsubj-pred-acomp
  "Find nominal subjects connected with adjectival complements via a verb."
  {:examples [{:text "It looks hungry."
               :triples '[[It looks hungry]]}]}
  (nlp/nsubj ?activity ?subject)
  (nlp/acomp ?activity ?a)
  (=> [?subject ?activity ?a]))

(defquery nsubj-pred-xcomp
  {:examples [{:text "He managed to enter the house."
               :triples '[[He managed enter]
                          [He enter house]]}]}
  (nlp/nsubj ?activity ?subject)
  (nlp/xcomp ?activity ?x)
  (l/conde
   [(nlp/advmod ?x ?object)]
   [(nlp/dobj ?x ?object)])
  (=> [?subject ?activity ?x]
      [?subject ?x ?object]))

(defquery nsubjpass-pred-agent
  {:examples [{:text "He is swayed by a warning."
               :triples '[[warning swayed He]]}]}
  (nlp/nsubjpass ?predicate ?subject)
  (nlp/agent ?predicate ?object)
  (=> [?object ?predicate ?subject]))

(defquery agent-ccomp-dobj
  {:examples [{:text "It is known by X that you do Y"
               :triples '[[X :about Y]]}]}
  (nlp/agent ?p ?a)
  (nlp/ccomp ?p ?c)
  (nlp/dobj ?c ?o)
  (=> [?a :about ?o]))

(defquery nsubj-advmod
  {:examples [{:text "The cat is often full."
               :triples '[[full :be often]]}]}
  (nlp/nsubj ?p ?s)
  (nlp/advmod ?p ?a)
  (l/conda
   [(l/pred ?p nlp/verb?)
    (=> [?s ?p ?a])]
   [(=> [?p :be ?a])]))

(defquery nsubjpass-ccomp
  "Extract a triple where two nouns are connected via some other word
  via `nsubjpass` or `ccomp` relations."
  {:examples [{:text "X and Y are connected as are B and C"
               :triples '[[C connected B]
                          [B connected C]
                          [Y connected X]
                          [X connected Y]]}]}
  (nlp/depends ?subj1 ?reln1 ?predicate)
  (nlp/depends ?subj2 ?reln2 ?predicate)
  (l/membero ?reln1 [:nsubjpass :ccomp])
  (l/membero ?reln2 [:nsubjpass :ccomp])

  (nlp/conj :and ?subj2 ?subj1)

  (=> [?subj1 ?predicate ?subj2]
      [?subj2 ?predicate ?subj1]))

(defquery prep-noun
  "Find nouns connected with prepositions to other nouns"
  {:examples [{:text    "They have a distance of 2 cm."
               :triples '[[distance :of cm]]}]}
  (nlp/prep ?p ?subj ?obj)
  (l/pred ?obj nlp/noun?)
  (l/pred ?subj nlp/noun?)
  (=> [?subj ?p ?obj]))

(defquery noun-prep-noun
  {:examples [{:text "X is in the same state as Y"
               :triples '[[X is state]
                          [X :in state]]}]}
  (nlp/nsubj ?activity ?subj)
  (nlp/prep ?p ?activity ?obj)
  (l/pred ?obj nlp/noun?)
  (l/pred ?subj nlp/noun?)
  (l/pred ?activity nlp/verb?)
  (=> [?subj ?p ?obj]
      [?subj ?activity ?obj]))

(defquery advmod-npadvmod-num
  "units and measurements"
  {:examples [{:text "A is 10 cm wide."
               :triples '[[wide :be cm]]}]}
  (l/conde
   [(nlp/advmod _ ?a)]
   [(nlp/word ?a)
    (l/pred ?a #(nlp/has-tags? #{"JJ"} %1))])
  (nlp/npadvmod ?a ?unit)
  (nlp/num ?unit _)
  (=> [?a :be ?unit]))

(defquery noun-num
  "measurements"
  {:examples [{:text "He has 5 apples."
               :triples '[[apples :be 5]]}]}
  (nlp/num ?unit ?n)
  (l/pred ?unit nlp/noun?)
  (l/pred ?n #(nlp/has-tags? #{"CD"} %1))
  (=> [?unit :be ?n]))

(defquery npossessive
  "Search for possessive words."
  {:examples [{:text "John's house is red."
               :triples '[[John :have house]
                          [house :of John]]}
              {:text "John's cheese"
               :triples '[[John :have cheese]
                          [cheese :of John]]}
              {:text "What is his birthday?"
               :triples '[[birthday :of his]
                          [his :have birthday]]}]}
  (nlp/poss ?object ?subject)
  (l/pred ?subject nlp/noun?)
  (l/pred ?object nlp/noun?)
  (=> [?subject :have ?object]
      [?object :of ?subject]))

(defquery prep-with-have
  {:examples [{:text "X and Y are separated with a distance of 1 cm."
               :triples '[[X :have distance]
                          [Y :have distance]]}]}
  (nlp/prep :with ?intermediate ?object)
  (nlp/nsubjpass ?intermediate ?subject)

  (l/pred ?subject nlp/noun?)
  (l/pred ?object nlp/noun?)

  (=> [?subject :have ?object]))
