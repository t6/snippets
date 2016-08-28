(defproject t6/snippets "0.1.0-SNAPSHOT"
  :description "An NLP library for Clojure"
  :url "https://github.com/t6/snippets"
  :license {:name "GNU Lesser General Public License"
            :url "http://www.gnu.org/licenses/lgpl.html"}

  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]

  :profiles {:dev {:dependencies [[midje "1.8.3"]
                                  [t6/snippets-corenlp "0.1.0-SNAPSHOT"]
                                  [t6/snippets-clearnlp "0.1.0-SNAPSHOT"]]
                   :plugins [[lein-midje-doc "0.0.24"]
                             [lein-midje "3.2"]
                             [lein-modules "0.3.11"]]}}

  :documentation {:files {"docs/index"
                          {:input "test/tobik/snippets/guide.clj"
                           :title "snippets"
                           :sub-title "An NLP library for Clojure"
                           :author "Tobias Kortkamp"
                           :email  "tobias.kortkamp@gmail.com"}}}

  :jvm-opts ["-Xms2g"]

  :modules {:inherited
            {:url "https://github.com/t6/snippets"
             :scm {:dir ".."}
             :license {:name "GNU Lesser General Public License"
                       :url "http://www.gnu.org/licenses/lgpl.html"}}}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.logic "0.8.10"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.macro "0.1.5"]

                 [rhizome "0.2.7"]

                 [camel-snake-kebab "0.4.0" :exclusions [com.keminglabs/cljx]]
                 [prismatic/plumbing "0.5.3"]])
