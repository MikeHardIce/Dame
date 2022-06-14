(defproject dame "0.1.5"
  :description "A Clojure implementation of the german checker game called Dame (Lady)"
  :url "https://github.com/MikeHardIce/Dame"
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [com.github.mikehardice/capra "0.0.3"]
                 [strigui "0.0.1-alpha29"]
                 [org.clojure/core.async "1.4.627"]]
  ;;:resource-paths ["resources/strigui-0.0.1-alpha28.jar"]
  :keep-non-project-classes true
  :main dame.core
  :aot [dame.core]
  :repl-options {:init-ns dame.core}
  :profiles {:uberjar {:aot [dame.core]}})