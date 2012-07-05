(ns cljs.coreloader
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
  (apply hash-map (mapcat #(if (keep-form? %) [(signature %) %] []) forms-seq)))

(defn core-forms-seq
  "Will load every form from core.cljs, except those who are defined in override-file
   override-file can be anything that can be coerced to a reader by io/reader"
  ([override-file & {:keys [replace-forms extra-file]}]
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
       (if extra-file
         (lazy-cat forms-filtered
                   (make-forms-seq extra-file))
         forms-filtered))))
