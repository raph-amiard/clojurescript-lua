;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns cljs.lua.common
  (:require [cljs.cljsloader :as cloader]
            [clojure.java.io :as io]
            [cljs.analyzer :as ana]))

(def replace-forms {'(extend-type js/Date) nil                   
                    '(extend-type array) '(deftype Array)
                    '(set! js/String.prototype.apply) nil
                    '(extend-type js/String) nil
                    '(deftype Keyword) nil
                    '(set! cljs.core.MultiFn.prototype.call) nil
                    '(set! cljs.core.MultiFn.prototype.apply) nil})

(def core-forms-seq
  (cloader/core-forms-seq (io/resource "core-lua.cljs")
                          :extra-file-before (io/resource "core-lua-init.cljs")
                          :extra-file-after (io/resource "core-lua-extra.cljs")
                          :replace-forms replace-forms))

 	  	
(defn new-env
  ([context] {:ns (@ana/namespaces ana/*cljs-ns*) :context context :locals {}})
  ([] (new-env :return)))

(def file-sep java.io.File/separator)

(defn get-cljs-dir []
  (str (System/getProperty "user.home") file-sep ".cljslua"))

(defn common-libs-path []
  (str (get-cljs-dir) file-sep "libs"))

(defn init-dirs []
  (.mkdirs (io/file (common-libs-path))))