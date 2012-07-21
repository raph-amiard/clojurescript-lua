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
                          :replace-forms replace-forms))

 	  	
(defn new-env
  ([context] {:ns (@ana/namespaces ana/*cljs-ns*) :context context :locals {}})
  ([] (new-env :return)))