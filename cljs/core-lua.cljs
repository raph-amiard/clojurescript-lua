(ns cljs.core)

(defn truth_
  "Internal - do not use!"
  [x]
  (js* "(~{x} ~= nil and ~{x} ~= false)"))

(defn ^boolean type_satisfies_
  "Internal - do not use!"
  [p x]
  (cond
   (aget p (lua/type x)) true
   (aget p "_") true
   :else false))

(defn aclone
  "Returns a javascript array, cloned from the passed in array"
  [array-like]
  (builtins/array-copy array-like))

(def array builtins/array)

(defn make-array
  ([size]
     (array))
  ([type size]
     (make-array size)))

(defn alength
  "Returns the length of the array. Works on arrays of all types."
  [array]
  (lua-length array))

(defn into-array
  ([aseq]
     (into-array nil aseq))
  ([type aseq]
     (reduce (fn [a x] (table/insert a x) a) (array) aseq)))
