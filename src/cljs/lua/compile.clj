(ns cljs.lua.compile
  (:require [clojure.java.io :as io]
            [cljs.lua.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.cljsloader :as cloader]
            [cljs.lua.common :as com])
  (:import  [java.io PrintWriter File FileInputStream FileOutputStream]))


(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name ".cljs")
                  (not= \. (first name))))
          (file-seq dir)))

(defn compile-dir [dir args]
  (doseq [f (cljs-files-in dir)]
    (doseq [form (cloader/make-forms-seq f)]
      (comp/emit (ana/analyze (ana/empty-env) form)))))

(defn compile-file [file args])

(defn -main [args]
  (ana/with-core-macros "/cljs/lua/core"
    (binding [ana/*cljs-ns* 'cljs.user
              ana/*cljs-static-fns* true]
      (let [src-file (io/file (args :source))]
        (println args)
        (if (.isDirectory src-file)
          (compile-dir src-file args)
          (compile-file src-file args))))))