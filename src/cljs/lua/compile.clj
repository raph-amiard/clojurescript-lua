(ns cljs.lua.compile
  (:require [clojure.java.io :as io]
            [cljs.lua.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.cljsloader :as cloader]
            [cljs.lua.common :as com])
  (:import  [java.io PrintWriter File FileInputStream FileOutputStream]))

(def ^:dynamic *cljs-files*)

(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name ".cljs")
                  (not= \. (first name))))
          (file-seq dir)))

(defn ns-decl [file]
  (let [first-form
        (->> file
             cloader/make-forms-seq
             first
             (ana/analyze (ana/empty-env)))]
    (if (= :ns (:op first-form))
      first-form
      nil)))

(defn make-files-map [dir]
  (apply hash-map (mapcat (fn [f] [((ns-decl f) :name) f]) (cljs-files-in dir))))

(defn compile-seq [seq]
  (doseq [form seq] (comp/emit (ana/analyze (ana/empty-env) form))))

(defn compile-file [file]
  (compile-seq (cloader/make-forms-seq file)))
    

(defn get-parent [file]
  (.getParentFile (io/file (.getCanonicalPath file))))

(defn compile-with-deps [file]
  (let [nsdecl (ns-decl file)
        requires (nsdecl :requires)]
    (doseq [[ns-alias ns-name] requires]
      (compile-with-deps (*cljs-files* ns-name)))
    (compile-file file)))

(defn compile-root-file [file]
  (binding [*cljs-files* (make-files-map (get-parent file))]
    (compile-with-deps file)))

(defn compile [file args]
  (let [nsd (ns-decl file)]
    (compile-seq com/core-forms-seq)
    ((if nsd compile-root-file compile-file) file)))

(defn -main [args]
  (ana/with-core-macros "/cljs/lua/core"
    (binding [ana/*cljs-ns* 'cljs.user
              ana/*cljs-static-fns* true]
      (let [src-file (io/file (args :source))]
        (if (.isDirectory src-file)
          (println "Input must be a cljsc file !")
          (compile src-file args))))))