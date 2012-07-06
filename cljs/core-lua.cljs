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

(def type builtins/type)

(defn ^boolean instance? [t o]
  (identical? t (type o)))

(extend-type default
  IHash
  (-hash [o] (lua/tonumber (string/sub (lua/tostring o) 10) 16)))

(deftype IndexedSeq [a i]
  
  ISeqable
  (-seq [this] this)

  ASeq
  ISeq
  (-first [_] (aget a i))
  (-rest [_] (if (< (inc i) (alength a))
               (IndexedSeq. a (inc i))
               (list)))

  INext
  (-next [_] (if (< (inc i) (alength a))
               (IndexedSeq. a (inc i))
               nil))

  ICounted
  (-count [_] (- (alength a) i))

  IIndexed
  (-nth [coll n]
    (let [i (+ n i)]
      (when (< i (alength a))
        (aget a i))))
  (-nth [coll n not-found]
    (let [i (+ n i)]
      (if (< i (alength a))
        (aget a i)
        not-found)))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  ICollection
  (-conj [coll o] (cons o coll))

  IReduce
  (-reduce [coll f]
    (if (counted? a)
      (ci-reduce a f (aget a i) (inc i))
      (ci-reduce coll f (aget a i) 0)))
  (-reduce [coll f start]
    (if (counted? a)
      (ci-reduce a f start i)
      (ci-reduce coll f start 0)))

  IHash
  (-hash [coll] (hash-coll coll))

  IReversible
  (-rseq [coll]
    (let [c (-count coll)]
      (if (pos? c)
        (RSeq. coll (dec c) nil)
        ()))))

(defn prim-seq
  ([prim]
     (prim-seq prim 0))
  ([prim i]
     (when-not (zero? (alength prim))
       (IndexedSeq. prim i))))

(extend-type table
  ISeqable
  (-seq [table] (array-seq table 0))

  ICounted
  (-count [a] (alength a))

  IIndexed
  (-nth
    ([table n]
       (if (< n (alength table)) (aget table n)))
    ([table n not-found]
       (if (< n (alength table)) (aget table n)
           not-found)))

  ILookup
  (-lookup
    ([table k]
       (aget table k))
    ([table k not-found]
       (-nth table k not-found)))

  IReduce
  (-reduce
    ([table f]
       (ci-reduce table f))
    ([table f start]
       (ci-reduce table f start))))

(defn add-to-string-hash-cache [k]
  (let [h (string/hashCode k)]
    (aset string-hash-cache k h)
    (set! string-hash-cache-count (inc string-hash-cache-count))
    h))

(defn hash
  ([o] (hash o true))
  ([o ^boolean check-cache]
     (if (and ^boolean (identical? (lua/type o) "string") check-cache)
       (check-string-hash-cache o)
       (-hash o))))

(defn lua-obj
  ([]
     (js* "{()}"))
  ([& keyvals] (lua/error "Not implemented !")))


(defn js-obj
  ([]
     (js* "({})"))
  ([& keyvals] (lua/error "Not implemented !")))

(def js-keys builtins/keys)

(defn js-delete [obj key]
    (js* "~{obj}[~{key}] = nil")
    nil)

(defn ^boolean string? [x]
  (and ^boolean (lua-string? x)
       (not (and (kw-or-sym? x)
                 (or (identical? (string/byte x 2) 144)
                     (identical? (string/byte x 2) 145))))))


(defn ^boolean keyword? [x]
  (and ^boolean
       (lua-string? x)
       (kw-or-sym? x)
       (identical? (string/byte x 3) 144)))

(defn ^boolean symbol? [x]
  (and ^boolean
       (lua-string? x)
       (kw-or-sym? x)
       (identical? (string/byte x 3) 145)))

(defn ^boolean number? [n]
  (identical? (lua/type n) "number"))

(defn ^boolean fn? [f]
  (identical? (lua/type f) "function"))

(defn ^boolean integer?
  "Returns true if n is an integer.  Warning: returns true on underflow condition."
  [n]
  (and (number? n)
       (coercive-= n (math/floor n))))


(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  ([coll n]
     (when-not (nil? coll)
       (if (satisfies? IIndexed coll)
         (-nth coll (math/floor n))
         (linear-traversal-nth coll (math/floor n)))))
  ([coll n not-found]
     (if-not (nil? coll)
       (if (satisfies? IIndexed coll)
         (-nth coll (math/floor n) not-found)
         (linear-traversal-nth coll (math/floor n) not-found))
       not-found)))


(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Uses IComparable if available and google.array.defaultCompare for objects
 of the same type and special-cases nil to be less than any other object."
  [x y]
  (cond
   (identical? x y) 0
   (nil? x) -1
   (nil? y) 1
   (identical? (type x) (type y)) (if (satisfies? IComparable x)
                                    (-compare x y)
                                    (builtins/compare x y))
   :else (throw (js/Error. "compare on non-nil objects of different types"))))

(defn sort
  "Returns a sorted sequence of the items in coll. Comp can be
   boolean-valued comparison funcion, or a -/0/+ valued comparator.
   Comp defaults to compare."
  ([coll]
   (sort compare coll))
  ([comp coll]
   (if (seq coll)
     (let [a (to-array coll)]
       ;; matching Clojure's stable sort, though docs don't promise it
       (builtins/sort a (fn->comparator comp))
       (seq a))
     ())))

(defn shuffle
  "Return a random permutation of coll"
  [coll]
  (let [a (to-array coll)]
    (builtins/shuffle a)
    (vec a)))
