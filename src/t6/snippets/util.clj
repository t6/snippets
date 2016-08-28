(ns t6.snippets.util
  (:refer-clojure :exclude [defn])
  (:require [schema.core :as s :refer [defn]]
            [lazymap.core :as lazymap]
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

(defn lazy-merge
  [& maps]
  (let [maps (reverse maps)]
    (reduce
     (fn [acc k]
       (lazymap/lazy-assoc
        acc
        k
        (loop [v ::not-found, maps maps]
          (if (= v ::not-found)
            (if (seq maps)
              (recur (k (first maps) ::not-found) (rest maps))
              (throw (Exception.)))
            v))))
     (lazymap/lazy-hash-map)
     (mapcat keys maps))))
