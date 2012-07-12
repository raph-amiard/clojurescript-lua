(ns cljs.lua.repl
  (:require [clojure.java.io :as io]
            [cljs.lua.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.coreloader :as cloader]
            [clojure.data.json :as json])
  (:import  [java.io PrintWriter File FileInputStream FileOutputStream]))

(def lua-interp "luajit-2.0.0-beta10")
(def ^:dynamic *repl-verbose* true)
(def ^:dynamic *repl-exec* true)
(def ^:dynamic *error-fatal?* false)
(def next-core-form (atom 0))

(def replace-forms {'(extend-type js/Date) nil                   
                    '(extend-type array) '(deftype Array)
                    '(set! js/String.prototype.apply) nil
                    '(extend-type js/String) nil
                    '(deftype Keyword) nil})

(def core-forms-seq
  (cloader/core-forms-seq (io/resource "core-lua.cljs")
                          :extra-file-before (io/resource "core-lua-init.cljs")
                          :replace-forms replace-forms))

(defn new-env [] {:ns (@ana/namespaces ana/*cljs-ns*) :context :return :locals {}})

(defn eval-core-forms [eval-fn n]
  (let [current-ns ana/*cljs-ns*]
    (binding [*repl-verbose* false] (eval-fn (new-env) '(ns cljs.core)))
    (doseq [form (take n (drop @next-core-form core-forms-seq))]
      (println "eval form " (take 2 form) "...")
      (eval-fn (new-env) form))
    (binding [*repl-verbose* false] (eval-fn (new-env) (list 'ns current-ns)))
    (swap! next-core-form + n)))

(def special-fns
  {'switch-verbose (fn [_] (set! *repl-verbose* (not *repl-verbose*)))
   'switch-exec (fn [_] (set! *repl-exec* (not *repl-exec*)))
   'eval-core-forms eval-core-forms})

(def special-fns-set (set (keys special-fns)))

(defn create-named-pipe [pfx]
  (let [pipe-path (-> (str "cljs_lua_" pfx "_")
                      (File/createTempFile ".fifo")
                      .getCanonicalPath)]
    (.waitFor (.exec (Runtime/getRuntime) (str "rm " pipe-path)))
    (.waitFor (.exec (Runtime/getRuntime) (str "mkfifo " pipe-path)))
    (File. pipe-path)))


(defn -main []  
  (ana/with-core-macros "/cljs/lua/core"
    (println "Cljs/Lua repl")    
    (binding [ana/*cljs-ns* 'cljs.user
              ana/*cljs-static-fns* true
              *repl-verbose* true
              *repl-exec* true]      
      (let [;; Lua subprocess
            pb (ProcessBuilder. [lua-interp "cljs/exec_server.lua"])
            lua-process (.start pb)
            
            ;; Read lua stdout
            rdr (io/reader (.getInputStream lua-process))

            ;; Named pipes to communicate with lua subproc
            pipe-in (create-named-pipe "ltj") 
            pipe-out (create-named-pipe "jtl")
            pipe-rdr (future (io/reader pipe-in))
            pipe-wr (future (io/writer pipe-out))

            ;; Function to analyze a form, emit lua code,
            ;; pass it to the lua subproc, and get back the result
            eval-form (fn [env form]
                        (let [lua-code (with-out-str (comp/emit (ana/analyze env form)))]
                          (when *repl-verbose*
                            (println "---- LUA CODE ----")
                            (println lua-code))
                          (when *repl-exec*
                            (binding [*out* @pipe-wr]
                              (println (json/json-str {:action :exec :body lua-code})))
                            (let [resp (json/read-json (.readLine @pipe-rdr))]
                              (if (= (:status resp) "OK")
                                (println (:body resp))
                                (do
                                  (println "ERROR : " (:body resp))
                                  (when *error-fatal?*
                                    (println lua-code)
                                    (.exit System))))))))]

        ;; Wait for exec server to be ready
         (.start (Thread. (fn [] (while true (let [l (.readLine rdr)] (when l (println l)))))))

        ;; Send it the two pipes names
        (binding [*out* (PrintWriter. (.getOutputStream lua-process))]
          (println (.getCanonicalPath pipe-in))
          (println (.getCanonicalPath pipe-out)))

        ;; Eval core.cljs forms
        (binding [*repl-verbose* false
                  *error-fatal?* true]
          (eval-core-forms eval-form 406))
        
        ;; Eval common ns formo
        (eval-form (new-env) '(ns cljs.user)) 
        
        ;; Repl loop
        (while true
          (.print System/out (str ana/*cljs-ns* " > "))
          (.flush (System/out))
          (let [env (new-env)
                form (read)
                special-fn? (and (seq? form) (contains? special-fns-set (first form)))]
            (if special-fn?
              (println (apply (special-fns (first form)) eval-form (rest form)))
              (eval-form env form))))))))
