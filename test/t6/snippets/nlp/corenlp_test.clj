(ns t6.snippets.nlp.corenlp-test
  (:use midje.sweet)
  (:require [t6.snippets.nlp.corenlp :as c]))

^{:refer t6.snippets.nlp.corenlp/load-pipeline-settings :added "0.1.0"}
(fact "load-pipeline-settings")

^{:refer t6.snippets.nlp.corenlp/pipeline :added "0.1.0"}
(fact "Returns a new StanfordCoreNLP object with the given settings.")

^{:refer t6.snippets.nlp.corenlp/def-annotation-accessors :added "0.1.0"}
(fact "Generates an accessor function for each `CoreAnnotation` inner class found
in each of the `annotation-classes`. Also adds core.typed type annotations to them, so
that they can be used by type annotated functions.

Use them to replace calls like

 `(.get document CoreAnnotations$BeginIndexAnnotation)`

with

  `(begin-index document)`

Note that the class name `CoreAnnotations$BeginIndexAnnotation` is stripped of
the suffix `Annotation` and its prefix `CoreAnnotations$` and `BeginIndex` is
converted to  kebab-case which results in a function name of `begin-index`.")

^{:refer t6.snippets.nlp.corenlp/throw-unless :added "0.1.0"}
(fact "throw-unless")
