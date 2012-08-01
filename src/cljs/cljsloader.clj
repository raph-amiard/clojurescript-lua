;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.cljsloader
  (:require [clojure.java.io :as io]))

(defn read-or-nil [rdr]
  (try (read rdr) (catch RuntimeException e nil)))

(defn make-forms-seq
  "Construct a lazy sequence of clojure forms from input f.
   f can be anything that can be coerced to a reader"
  [f]
  (letfn [(forms-seq [rdr]
            (let [form (read-or-nil rdr)]
              (if (nil? form) []
                  (lazy-seq (cons form (forms-seq rdr))))))]
    (forms-seq (java.io.PushbackReader. (io/reader f)))))

(defn keep-form? [form]
  (contains? #{'ns 'def 'defn 'deftype 'extend-type} (first form)))

(defn signature [form]
  (if (= 'defn (first form))
    `(def ~(second form))
    (take 2 form)))

(defn make-override-map [forms-seq]
  (apply hash-map (mapcat (fn [a] [(signature a) a]) forms-seq)))

(defn core-forms-seq
  "Will load every form from core.cljs, except those who are defined in override-file
   override-file can be anything that can be coerced to a reader by io/reader"
  ([override-file & {:keys [replace-forms extra-file-before extra-file-after]}]
     (let [core-forms (make-forms-seq (io/resource "cljs/core.cljs")) 
           override-map (-> override-file make-forms-seq make-override-map)
           replace-forms (or replace-forms {})
           forms-override (for [form core-forms]
                            (let [sig (signature form)]
                              (cond
                               (contains? override-map sig) (override-map sig)
                               (contains? replace-forms sig) (override-map (replace-forms sig))
                                :else form)))
           forms-filtered (remove nil? forms-override)]
       (lazy-cat (if extra-file-before (make-forms-seq extra-file-before) [])
                 forms-filtered
                 (if extra-file-after (make-forms-seq extra-file-after) [])))))