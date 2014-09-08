(defproject t6/snippets "0.1.0-SNAPSHOT"
  :description "An NLP library for Clojure"
  :url "https://github.com/t6/snippets"
  :license {:name "GNU Lesser General Public License"
	    :url "http://www.gnu.org/licenses/lgpl.html"}

  :profiles {:dev {:dependencies [[midje "1.6.3"]
                                  [t6/snippets-corenlp "0.1.0-SNAPSHOT"]
                                  [t6/snippets-clearnlp "0.1.0-SNAPSHOT"]]
		   :plugins [[lein-midje-doc "0.0.24"]
			     [lein-midje "3.1.3"]
                             [lein-modules "0.3.8"]]}}

  :documentation {:files {"docs/index"
			  {:input "test/tobik/snippets/guide.clj"
			   :title "snippets"
			   :sub-title "An NLP library for Clojure"
			   :author "Tobias Kortkamp"
			   :email  "tobias.kortkamp@gmail.com"}}}

  :modules {:inherited
            {:url "https://github.com/t6/snippets"
             :scm {:dir ".."}
             :license {:name "GNU Lesser General Public License"
                       :url "http://www.gnu.org/licenses/lgpl.html"}}}

  :dependencies [[org.clojure/clojure "1.6.0"]
		 [org.clojure/core.logic "0.8.8"]
		 [org.clojure/tools.reader "0.8.8"]
		 [org.clojure/tools.macro "0.1.5"]

                 [rhizome "0.2.1"]
		 
		 [camel-snake-kebab "0.2.4"]
		 [prismatic/plumbing "0.3.3"]])
