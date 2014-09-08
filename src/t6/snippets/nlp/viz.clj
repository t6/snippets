(ns t6.snippets.nlp.viz
  (:refer-clojure :exclude [defn])
  (:require [schema.core :as s :refer (defn)]
            [plumbing.core :refer (fnk)]
            [clojure.string :as str]
            [rhizome.dot :as dot]
            [rhizome.viz :as viz]
            [t6.snippets.nlp :as nlp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visualising semantic graphs

(defn ^:private render-semantic-graph
  [f graph]
  (f
   (:nodes graph)
   (fn [node]
     (mapv (fn [[_ x]] x)
           (-> graph (get :edges) (get node))))

   :node->descriptor
   (fnk [token tag sentence index :as word]
     (merge
      {:label (format "%s-%s-%s:%s" token (name tag) sentence index)
       :shape "box"}
      (cond
       (nlp/noun? word)
       {:style     "filled"
        :fillcolor "#FAC400"}

       (nlp/verb? word)
       {:style     "filled"
        :fillcolor "#5DE66D"}

       (nlp/pronoun? word)
       {:style "filled"})))

   :edge->descriptor
   (fn [src dest]
     (when-let [relation (some (fn [[rel d]]
                                 (if (= d dest)
                                   rel))
                               (-> graph (get :edges) (get src)))]
       {:label (if (keyword? relation)
                 (name relation)
                 (str/join "_" (map name relation)))}))))

(defn semantic-graph->dot :- s/Str
  [graph :- nlp/SemanticGraph]
  (render-semantic-graph rhizome.dot/graph->dot graph))

(defn show-semantic-graph
  [graph :- nlp/SemanticGraph]
  (render-semantic-graph rhizome.viz/view-graph graph)
  nil)
