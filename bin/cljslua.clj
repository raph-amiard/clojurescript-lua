(require '[cljs.lua.repl :as repl])
(require '[cljs.lua.compile :as comp])

(def commands {"compile" comp/-main
               "repl"    repl/-main})

(let [args *command-line-args*
      cmd-func (commands (first args))]
  (if cmd-func
    (cmd-func (rest args))
    (println "Unknown command : " (first args))))
