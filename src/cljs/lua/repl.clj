(ns cljs.lua.repl
  (:require [clojure.java.io :as io]
            [cljs.lua.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.coreloader :as cloader]
            [clojure.data.json :as json])
  (:import  [java.io PrintWriter File FileInputStream FileOutputStream]))

(def lua-interp "lua")
(def ^:dynamic *repl-verbose* true)
(def ^:dynamic *repl-exec* true)
(def next-core-form (atom 0))

(def core-forms-seq
  (cloader/core-forms-seq (io/resource "core-lua.cljs")
                          (io/resource "core-lua-extra.cljs")))

(defn new-env [] {:ns (@ana/namespaces ana/*cljs-ns*) :context :return :locals {}})

(defn eval-core-forms [eval-fn n]
  (doseq [form (take n (drop @next-core-form core-forms-seq))]
    (println "eval form " (take 2 form) "...")
    (let [res (eval-fn (new-env) form)]
      (println "---- LUA CODE ----")
      (println (:lua-code res)))
    (println "success !"))
    (swap! next-core-form + n))

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
              *repl-verbose* true
              *repl-exec* true]      
      (let [;; Function to create a new env

            ;; Lua subprocess
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
                          (when *repl-exec*
                            (binding [*out* @pipe-wr]
                              (println (json/json-str {:action :exec :body lua-code}))))
                          (let [raw-resp (.readLine @pipe-rdr)]
                            {:lua-code lua-code :response (when *repl-exec* (json/read-json raw-resp))})))]

        ;; Wait for exec server to be ready
         (.start (Thread. (fn [] (while true (let [l (.readLine rdr)] (when l (println l)))))))

        ;; Send it the two pipes names
        (binding [*out* (PrintWriter. (.getOutputStream lua-process))]
          (println (.getCanonicalPath pipe-in))
          (println (.getCanonicalPath pipe-out)))

        ;; Eval core.cljs forms
        (eval-core-forms eval-form 65)
        
        ;; Eval common ns form
        (eval-form (new-env) '(ns cljs.user)) 
        
        ;; Repl loop
        (while true
          (.print System/out (str ana/*cljs-ns* " > "))
          (.flush (System/out))
          (let [env (new-env)
                form (read)
                special-fn? (contains? special-fns-set (first form))
                res (when (not special-fn?) (eval-form env form))]
            (if special-fn?
              (println (apply (special-fns (first form)) eval-form (rest form)))
              (do
                (when *repl-verbose*
                  (println "---- LUA CODE ----")
                  (println (:lua-code res)))
                (let [resp (:response res)]
                  (when resp
                    (if (= (:status resp) "OK")
                      (println (:body resp))
                      (println "ERROR"))))))))))))0