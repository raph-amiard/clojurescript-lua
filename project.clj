(defproject clojurescript-lua "0.1.0"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.2"]]
  :plugins [[lein-swank "1.4.0"]
            [lein-pprint "1.1.1"]]
  :extra-classpath-dirs ["libclojurescript.jar"]
  :resources-path "cljs"
  :source-path "src"
  :main cljs.lua.compile
)
