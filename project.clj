(defproject t6/snippets "0.1.0-SNAPSHOT"
  :description "An NLP library for Clojure"
  :url "https://github.com/t6/snippets"
  :license {:name "GNU Lesser General Public License"
	    :url "http://www.gnu.org/licenses/lgpl.html"}

  :profiles {:dev {:dependencies [[midje "1.6.3"]]
		   :plugins [[lein-midje-doc "0.0.24"]
			     [lein-midje "3.1.3"]]}}

  :documentation {:files {"docs/index"
			  {:input "test/tobik/snippets/guide.clj"
			   :title "snippets"
			   :sub-title "An NLP library for Clojure"
			   :author "Tobias Kortkamp"
			   :email  "tobias.kortkamp@gmail.com"}}}

  :dependencies [[org.clojure/clojure "1.6.0"]
		 [org.clojure/core.logic "0.8.8"]
		 [org.clojure/tools.reader "0.8.8"]
		 [org.clojure/tools.macro "0.1.5"]

		 [t6/graph-transform "0.1.0"]

		 [rhizome "0.2.1"]

		 [com.clearnlp/clearnlp "2.0.2"]
		 [com.clearnlp/clearnlp-dictionary "1.0"]
		 [com.clearnlp/clearnlp-general-en-pos "1.1"]
		 [com.clearnlp/clearnlp-general-en-dep "1.2"]
		 [com.clearnlp/clearnlp-general-en-srl "1.1"]

		 [org.apache.opennlp/opennlp-tools "1.5.3"]
		 [edu.washington.cs.knowitall/opennlp-tokenize-models "1.5"]
		 [edu.washington.cs.knowitall/opennlp-sent-models "1.5"]

		 [camel-snake-kebab "0.2.4"]
		 [prismatic/plumbing "0.3.3"]

		 [edu.stanford.nlp/stanford-corenlp "3.4.1"]
		 [edu.stanford.nlp/stanford-corenlp "3.4"
		  :classifier "models"]])
