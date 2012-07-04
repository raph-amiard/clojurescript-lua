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
  (contains? #{'ns 'def 'defn} (first form)))

(defn signature [form]
  (if (= 'defn (first form))
    `(def ~(second form))
    (take 2 form)))

(defn make-override-map [forms-seq]
  (apply hash-map (mapcat #(if (keep-form? %) [(signature %) %] []) forms-seq)))

(defn core-forms-seq
  "Will load every form from core.cljs, except those who are defined in override-file
   override-file can be anything that can be coerced to a reader by io/reader
   Will subsequently load forms in extra-file, if provided"
  ([override-file]
     (let [core-forms (make-forms-seq (io/resource "cljs/core.cljs")) 
           override-map (-> override-file make-forms-seq make-override-map)]
       (for [form core-forms]
         (let [sig (signature form)]
           (if (contains? override-map sig)
             (override-map sig)
             form)))))
  ([override-file extra-file]
     (lazy-cat (core-forms-seq override-file)
               (make-forms-seq extra-file))))