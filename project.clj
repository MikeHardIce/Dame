(defproject dame "0.1.4"
  :description "A Clojure implementation of the german checker game called Dame (Lady)"
  :url "https://github.com/MikeHardIce/Dame"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [com.github.mikehardice/capra "0.0.1-alpha4"]
                 [strigui "0.0.1-alpha26"]
                 [org.clojure/core.async "1.4.627"]]
  :keep-non-project-classes true
  :main dame.core
  :aot [dame.core]
  :repl-options {:init-ns dame.core}
  :profiles {:uberjar {:aot [dame.core] }})