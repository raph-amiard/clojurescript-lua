(defproject libclojurescript "0.1.0"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :plugins [[lein-swank "1.4.0"]
            [lein-pprint "1.1.1"]]
  :source-path "src/clj"
  :jar-name "libclojurescript.jar"
  :extra-classpath-dirs ["src/cljs"]
)
