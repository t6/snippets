(ns t6.snippets.util
  (:refer-clojure :exclude [defn])
  (:require [schema.core :as s :refer (defn)]
	    [camel-snake-kebab.core :refer (->kebab-case)]))

(defn enum->keyword :- (s/maybe s/Keyword)
  [e :- (s/maybe Enum)]
  (when e
    (if-let [s (.toLowerCase (str e))]
      (keyword (->kebab-case s)))))

(defn parse-int :- (s/maybe s/Int)
  [integer :- s/Str, base :- s/Int]
  (try
    (Long/parseLong integer (int base))
    (catch NumberFormatException e)))
