;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.lua.compile
  (:require [clojure.java.io :as io]
            [cljs.lua.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.cljsloader :as cloader]
            [cljs.lua.common :as com])
  (:import  [java.io PrintWriter File FileInputStream FileOutputStream]))

(def ^:dynamic *cljs-files*)

(defn emit-lib-header [ns]
  (println "-- CLJS/LUA " ns))

(defn files-in
  "Return a sequence of all files with given extension in the given directory."
  [ext dir]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name (str "." ext))
                  (not= \. (first name))))
          (file-seq dir)))

(def cljs-files-in
  (partial files-in "cljs"))

(defn lib-ns [lib-file]
  (let [first-line (binding [*in* (clojure.java.io/reader lib-file)]
                     (read-line))]
    (if (.startsWith first-line "-- CLJS/LUA")
      (symbol (nth (.split first-line " ") 3)))))

(def lib-files-map
  (apply hash-map
         (mapcat (fn [f] [(lib-ns f) f])
                 (files-in "cljlib" (io/file (com/common-libs-path))))))

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
  (doseq [form seq]
    (comp/emit (ana/analyze (ana/empty-env) form))))

(defn compile-file [file optmap]
  (compile-seq (cloader/make-forms-seq file)))

(defn get-parent [file]
  (.getParentFile (io/file (.getCanonicalPath file))))

(defn compile-with-deps [file optmap]
  (let [nsdecl (ns-decl file)
        requires (nsdecl :requires)]
    (doseq [[ns-alias ns-name] requires]
      (if (*cljs-files* ns-name)
        (compile-with-deps (*cljs-files* ns-name) optmap)
        (if (lib-files-map ns-name)
          (println (slurp (lib-files-map ns-name)))
          (throw (Exception. (str "Dependency not found : " ns-name "!"))))))
    (compile-file file optmap)))

(defn compile-root-file [file {:keys [no-deps] :as optmap}]
  (binding [*cljs-files* (make-files-map (get-parent file))]
    (compile-with-deps file optmap)))

(defn -compile [file {:keys [no-deps as-lib] :as optmap}]
  (let [nsd (ns-decl file)]
    ;; Adding builtins
    (if-not no-deps (println (slurp (io/resource "builtins.lua"))))
    ;; Adding core.cljs
    (if-not no-deps (println (slurp com/core-lib-path)))
    (if as-lib (emit-lib-header (second (nsd :form))))
    ;; Compile main file and deps
    ((if (and nsd (not no-deps)) compile-root-file compile-file) file optmap)))

(defn remove-dots [s]
  (.replaceAll (str s) "\\." "_"))

(defn lib-file-name [src-file {:keys [as-lib]}]
  (let [nsd (ns-decl src-file)
        fname (if nsd
                (remove-dots (comp/munge (second (nsd :form))))
                (throw (Exception. "No ns decl")))]
    (str (com/common-libs-path) com/file-sep fname ".cljlib")))

(defn mk-out-file [src-file {:keys [out-file as-lib] :as optmap}]
  (let [o
        (if out-file out-file
            (if as-lib (lib-file-name src-file optmap) *out*))]
    (println o)
    (io/writer o)))
  
(defn -main [src-file & {:keys [out-file as-lib] :as optmap}]
    (binding [ana/*cljs-ns* 'cljs.user
              ana/*cljs-static-fns* true
              comp/*ns-emit-require* false]
      (let [src-file (io/file src-file)]
        (if (.isDirectory src-file)
          (println "Input must be a cljsc file !")
          (binding [*out* (mk-out-file src-file optmap)]
            (-compile src-file (if as-lib (assoc optmap :no-deps true) optmap)))))))

(defn compile-core []
  (let [core-lib (io/file com/core-lib-path)]
    (when-not (.exists core-lib)
      (println "Compiling core ...")
      (binding [ana/*cljs-ns* 'cljs.user
                ana/*cljs-static-fns* true
                comp/*ns-emit-require* false
                *out* (io/writer core-lib)]
        (emit-lib-header 'cljs.core)
        (compile-seq com/core-forms-seq)))))