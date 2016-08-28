(ns t6.snippets.span
  "Utility functions for transforming and modifying spans."
  (:refer-clojure :exclude [subs defn update])
  (:require [schema.core :as s :refer [defn]]))

(def Span
  "A span `[x y]` describes a range starting at `x` and ending at `y`."
  (s/both [(s/one s/Int "x")
           (s/one s/Int "y")]
          (s/pred (fn [[x y]] (>= y x 0))
                  '>=0)))

(def SpanMap
  "A map with a `:span` key."
  {:span Span, s/Any s/Any})

(def SpanLike
  (s/either SpanMap Span))

(def Point
  "A point inside a span"
  (s/both s/Int (s/pred #(>= %1 0) '>=0)))

(defn get-span :- Span
  "Validates and returns the given span. Throws exception if
validation fails.

  (get-span {:span [1 2]}) => [1 2]
  (get-span [1 2]) => [1 2]
  (get-span [5 1]) => (throws ExceptionInfo \"Not a valid span\")"
  {:added "0.1.0"}
  [span :- SpanLike]
  (:span span span))

(defn inside? :- Boolean
  "Test whether span2 is inside span1

  (inside? [0 10] [2 3]) => true
  (inside? {:span [0 10]} [2 3]) => true
  (inside? [2 3] [0 10]) => false
  (inside? [2 3] [-1 -1]) => (throws ExceptionInfo \"Not a valid span\")"
  {:added "0.1.0"}
  [span1 :- SpanLike, span2 :- SpanLike]
  (let [[x1 y1] (get-span span1)
        [x2 y2] (get-span span2)]
    (<= x1 x2 y2 y1)))

(defn point-inside? :- Boolean
  "Tests whether `point` is inside `span`

  (point-inside? [0 10] 5) => true
  (point-inside? [0 10] 50) => false
  (point-inside? [0 10] 10) => false
  (point-inside? [0 10] 0) => true
  (point-inside? [0 10] -1) => (throws ExceptionInfo \"Not a valid point\")
  (point-inside? {:span [0 10]} 0) => true"
  {:added "0.1.0"}
  [span :- SpanLike, point :- Point]
  (let [[x y] (get-span span)]
    (and (<= x point)
         (< point y))))

(defn length :- Point
  "Returns the length of a span

  (length [0 5]) => 5
  (length [-1 5]) => (throws ExceptionInfo \"Not a valid span\")
  (length [10 20]) => 10"
  {:added "0.1.0"}
  [span :- SpanLike]
  (let [[x y] (get-span span)]
    (- y x)))

(defn update :- Span
  "Update `span` by adding `dspan` to it. `dspan` may contain
negative integers.

  (update {:span [0 10]} [5 5]) => [5 15]
  (update [0 10] {:span [5 5]}) => [5 15]
  (update {:span [0 10]} {:span [5 5]}) => [5 15]
  (update [0 10] [5 5]) => [5 15]
  (update [0 10] [0 -1]) => [0 9]
  (update [0 10] [-5 0]) => [0 10]
  (update [0 10] [-5 -10]) => [0 0]
  (update [0 10] [-10 -10]) => [0 0]
  (update [0 0] [-1 -1]) => [0 0]"
  {:added "0.1.0"}
  [span :- SpanLike
   dspan :- (s/either {:span Span}
                      [(s/one s/Int "dx") (s/one s/Int "dy")])]
  (let [[x y] (get-span span)
        [dx dy] (:span dspan dspan)]
    [(int (max 0 (+ x dx)))
     (int (max 0 (+ y dy)))]))

(defn project-point :- Point
  "Moves the cursor position `point` into the boundaries of `span`.

  (project-point [0 5] 3) => 3
  (project-point [1 5] 3) => 2
  (project-point [1 5] 5) => (throws ExceptionInfo \"Point not inside span\")
  (project-point {:span [109 1425]} 125) => 16"
  {:added "0.1.0"}
  [span :- SpanLike, point :- Point]
  (let [span (get-span span)]
    (when-not (point-inside? span point)
      (throw (ex-info "Point not inside span" {:span span, :point point})))
    (- point (nth span 0))))

(defn project :- Span
  "Moves the smaller of two spans into the boundaries of the larger span.

  (project {:span [1 10]} {:span [1 11]}) => [2 11]
  (project [1 10] [1 11]) => [2 11]
  (project [1 10] [1 10]) => [1 10]
  (project [1 100] [2 6]) => [3 7]"
  {:added "0.1.0"}
  [span1 :- SpanLike, span2 :- SpanLike]
  (let [[[a b] [x y]] (sort-by length [(get-span span1)
                                       (get-span span2)])]
    (assert (and a b x y))
    (if (= [a b] [x y])
      [a b]
      (let [span (update [x x] [a b])]
        (assert (inside? [x y] span))
        span))))

(defn difference :- (s/maybe Span)
  "Returns the difference between two spans or `nil` if they
intersect i.e. if there is no possible space between them.

  (difference {:span [10 50]} {:span [1 10]}) => [10 10]
  (difference [4 50] [1 10])  =>  because the spans intersect
  (difference [10 50] [1 10]) => [10 10]
  (difference [1 10] [15 50]) => [10 15]"
  {:added "0.1.0"}
  [span1 :- SpanLike, span2 :- SpanLike]
  (let [[[_ x] [y _]] (sort [(get-span span1)
                             (get-span span2)])]
    (when (and x y (>= y x))
      [x y])))

(defn union :- Span
  "Returns the union of two spans. The new span includes both
spans.

  (union {:span [4 50]} {:span [1 10]}) => [1 50]
  (union [4 50] [1 10]) => [1 50]
  (union [0 10] [30 40]) => [0 40]"
  {:added "0.1.0"}
  [span1 :- SpanLike, span2 :- SpanLike]
  (let [[[a b] [c d]] (sort [(get-span span1)
                             (get-span span2)])]
    (assert (and a b c d))
    [(int (min a c)) (int (max b d))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that operate on collections that contain spans

(def PointSpanPair
  [(s/one Point "point") (s/one SpanLike "span")])

(defn span-at-point :- (s/maybe PointSpanPair)
  "Given a sequence of spans returns a pair `[i span]` where
`i`/`span` is the index/span `point` is inside of.  Always returns the
first match.  May return `nil` if there is no such pair.

  (span-at-point [[1 3] [1 10]] 3) => [1 [1 10]]
  (span-at-point [[1 3] [1 10]] 2) => [0 [1 3]]
  (span-at-point [{:span [1 3]} {:span [1 10]}] 3) => [1 {:span [1 10]}]
  (span-at-point [[-1 10]] 3) => (throws ExceptionInfo \"Not a valid span\")"
  {:added "0.1.0"}
  [spans :- [SpanLike], point :- Point]
  (->> spans
       (map-indexed vector)
       (some (fn [[i span*]]
               (if-let [span (get-span span*)]
                 (if (point-inside? span point)
                   [i span*]))))))

(defn subs :- s/Str
  "Like `clojure.core/subs` but takes a span as start and end
point of the substring. This function will not throw an exception when
the indices do not match the string, but will return as much as
possible of the string or an empty string.

  (subs \"Hello\" {:span [4 5]}) => \"o\"
  (subs \"Hello\" [4 5]) => \"o\"
  (subs \"Hello\" [50 100]) => \"\"
  (subs \"Hello\" [0 100]) => \"Hello\""
  {:added "0.1.0"}
  [s :- s/Str, span :- SpanLike]
  (let [n (count s)
        [start end] (get-span span)]
    (clojure.core/subs s
                       (if (>= start n) n start)
                       (if (<= end n) end n))))
