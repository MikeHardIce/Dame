(defproject dame "0.1.6"
  :description "A Clojure implementation of the german checker game called Dame (Lady)"
  :url "https://github.com/MikeHardIce/Dame"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.github.mikehardice/capra "0.0.10"]
                 [strigui "0.0.1-alpha32"]
                 [org.clojure/core.async "1.4.627"]]
  ;;:resource-paths ["resources/strigui-0.0.1-alpha32.jar"]
  :keep-non-project-classes true
  :main dame.core
  :aot [dame.core]
  :repl-options {:init-ns dame.core}
  :profiles {:uberjar {:jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.skip-macros=true"]
                       :main dame.core
                       :aot [dame.core]}})