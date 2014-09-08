(ns t6.snippets.span-test
  (:refer-clojure :exclude [subs])
  (:use midje.sweet)
  (:require [schema.core :as s]
            [t6.snippets.span :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(s/set-fn-validation! true)

^{:refer t6.snippets.span/get-span :added "0.1.0"}
(fact "Validates and returns the given span. Throws exception if
validation fails."
  (get-span {:span [1 2]}) => [1 2]
  (get-span [1 2]) => [1 2]
  (get-span [5 1]) => (throws ExceptionInfo #"not match schema"))

^{:refer t6.snippets.span/inside? :added "0.1.0"}
(fact "Test whether span2 is inside span1"
  (inside? [0 10] [2 3]) => true
  (inside? {:span [0 10]} [2 3]) => true
  (inside? [2 3] [0 10]) => false
  (inside? [2 3] [-1 -1]) => (throws ExceptionInfo #"not match schema"))

^{:refer t6.snippets.span/point-inside? :added "0.1.0"}
(fact "Tests whether `point` is inside `span`"
  (point-inside? [0 10] 5) => true
  (point-inside? [0 10] 50) => false
  (point-inside? [0 10] 10) => false
  (point-inside? [0 10] 0) => true
  (point-inside? [0 10] -1) => (throws ExceptionInfo #"not match schema")
  (point-inside? {:span [0 10]} 0) => true)

^{:refer t6.snippets.span/length :added "0.1.0"}
(fact "Returns the length of a span"
  (length [0 5]) => 5
  (length [-1 5]) => (throws ExceptionInfo #"not match schema")
  (length [10 20]) => 10)

^{:refer t6.snippets.span/update :added "0.1.0"}
(fact "Update `span` by adding `dspan` to it. `dspan` may contain
negative integers."
  (update {:span [0 10]} [5 5]) => [5 15]
  (update [0 10] {:span [5 5]}) => [5 15]
  (update {:span [0 10]} {:span [5 5]}) => [5 15]
  (update [0 10] [5 5]) => [5 15]
  (update [0 10] [0 -1]) => [0 9]
  (update [0 10] [-5 0]) => [0 10]
  (update [0 10] [-5 -10]) => [0 0]
  (update [0 10] [-10 -10]) => [0 0]
  (update [0 0] [-1 -1]) => [0 0])

^{:refer t6.snippets.span/project-point :added "0.1.0"}
(fact "Moves the cursor position `point` into the boundaries of `span`."
  (project-point [0 5] 3) => 3
  (project-point [1 5] 3) => 2
  (project-point [1 5] 5) => (throws ExceptionInfo "Point not inside span")
  (project-point {:span [109 1425]} 125) => 16)

^{:refer t6.snippets.span/project :added "0.1.0"}
(fact "Moves the smaller of two spans into the boundaries of the larger span."
  (project {:span [1 10]} {:span [1 11]}) => [2 11]
  (project [1 10] [1 11]) => [2 11]
  (project [1 10] [1 10]) => [1 10]
  (project [1 100] [2 6]) => [3 7])

^{:refer t6.snippets.span/difference :added "0.1.0"}
(fact "Returns the difference between two spans or `nil` if they
intersect i.e. if there is no possible space between them."
  (difference {:span [10 50]} {:span [1 10]}) => [10 10]
  (difference [4 50] [1 10])  => nil ;; because the spans intersect
  (difference [10 50] [1 10]) => [10 10]
  (difference [1 10] [15 50]) => [10 15])

^{:refer t6.snippets.span/union :added "0.1.0"}
(fact "Returns the union of two spans. The new span includes both
spans."
  (union {:span [4 50]} {:span [1 10]}) => [1 50]
  (union [4 50] [1 10]) => [1 50]
  (union [0 10] [30 40]) => [0 40])

^{:refer t6.snippets.span/span-at-point :added "0.1.0"}
(fact "Given a sequence of spans returns a pair `[i span]` where
`i`/`span` is the index/span `point` is inside of.  Always returns the
first match.  May return `nil` if there is no such pair."
  (span-at-point [[1 3] [1 10]] 3) => [1 [1 10]]
  (span-at-point [[1 3] [1 10]] 2) => [0 [1 3]]
  (span-at-point [{:span [1 3]} {:span [1 10]}] 3) => [1 {:span [1 10]}]
  (span-at-point [[-1 10]] 3) => (throws ExceptionInfo #"not match schema"))

^{:refer t6.snippets.span/subs :added "0.1.0"}
(fact "Like `clojure.core/subs` but takes a span as start and end
point of the substring. This function will not throw an exception when
the indices do not match the string, but will return as much as
possible of the string or an empty string."
  (subs "Hello" {:span [4 5]}) => "o"
  (subs "Hello" [4 5]) => "o"
  (subs "Hello" [50 100]) => ""
  (subs "Hello" [0 100]) => "Hello")
