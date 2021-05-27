(defproject dame "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clojure2d "1.4.3"]
                 [strigui "0.0.1-alpha6"]]
  
  :main dame.core
  :aot [dame.core]
  :repl-options {:init-ns dame.core})
