(defproject clojurescript-lua "0.1.0"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :plugins [[lein-swank "1.4.0"]
            [lein-pprint "1.1.1"]]
  :extra-classpath-dirs ["lib/libclojurescript.jar"]
  :source-path "src"
)
