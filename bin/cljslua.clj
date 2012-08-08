(require '[cljs.lua.repl :as repl])
(require '[cljs.lua.config :as conf])
(require '[cljs.lua.compile :as comp])
(require '[cljs.lua.common :as com])
(require '[cljs.analyzer :as ana])

(def commands {"compile" comp/-main
               "repl"    repl/-main})

(defn keywordize-args [args]
  (for [arg args]
    (if (.startsWith arg ":") (keyword (subs arg 1)) arg)))

(ana/with-core-macros "/cljs/lua/core"  
  (let [args (keywordize-args *command-line-args*)
        cmd-func (commands (first args))]
    (conf/load-config)
    (com/init-dirs)
    (comp/compile-core)
    (if cmd-func
      (apply cmd-func (rest args))
      (println "Unknown command : " (first args)))))