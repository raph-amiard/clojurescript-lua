(ns cljs.core)

(defn ^seq rest
  "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
  [coll]
  (set! *unchecked-if* true) 
  (if-not (nil? coll)
    (if (satisfies? ISeq coll)
      (-rest coll)
      (let [s (seq coll)]
        (if-not (nil? s)
          (-rest s)
          ())))
    ()))

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
  (-hash [o] (lua/tonumber (string/sub (lua/tostring o) 10) 16))
  Object
  (toString [o] (lua/tostring o)))

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

(deftype Array []
  Object
  (toString [t] (builtins/array-to-string t))
  
  ISeqable
  (-seq [arr] (array-seq arr 0))

  ICounted
  (-count [a] (alength a))

  IIndexed
  (-nth [arr n]
    (if (< n (alength arr)) (aget arr n)))  
  (-nth [arr n not-found]
    (if (< n (alength arr)) (aget arr n)
        not-found))

  ILookup
  (-lookup [arr k]
    (aget arr k))
  (-lookup [arr k not-found]
    (-nth arr k not-found))

  IReduce
  (-reduce [arr f]
    (ci-reduce arr f))
  (-reduce [arr f start]
    (ci-reduce arr f start)))

(defn add-to-string-hash-cache [k]
  (let [h (string/hashCode k)]
    (asetg string-hash-cache k h)
    (set! string-hash-cache-count (inc string-hash-cache-count))
    h))

(defn check-string-hash-cache [k]
  (when (> string-hash-cache-count 255)
    (set! string-hash-cache (js-obj))
    (set! string-hash-cache-count 0))
  (let [h (agetg string-hash-cache k)]
    (if-not (nil? h)
      h
      (add-to-string-hash-cache k))))

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
                 (or (identical? (string/byte x 3) 144)
                     (identical? (string/byte x 3) 145))))))


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

(defn- str*
  "Internal - do not use!"
  ([] "")
  ([x] (cond
        (nil? x) ""
        :else (toString x)))
  ([x & ys]
     ((fn [sb more]
        (if more
          (recur (append sb (str* (first more))) (next more))
          (str* sb)))
      (append (StringBuffer.) (str* x)) ys)))


(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  ([] "")
  ([x] (cond
        (symbol? x) (string/sub x 5)
        (keyword? x) (str* ":" (string/sub x 5))
        (nil? x) ""
        :else (toString x)))
  ([x & ys]
     ((fn [sb more]
        (if more
          (recur (append sb (str (first more))) (next more))
          (str* sb)))
      (append (StringBuffer.) (str x)) ys)))


(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([s start] (string/sub s (inc start)))
  ([s start end] (string/sub s (inc start) end)))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  ([name] (cond (symbol? name) name
                (keyword? name) (str* "\\239\\183\\144" "'" (subs name 2))
                :else (str* "\\239\\183\\144" "'" name)))
  ([ns name] (symbol (str* ns "/" name))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  ([name] (cond (keyword? name) name
                (symbol? name) (str* "\\239\\183\\144" "'" (subs name 2))
                :else (str* "\\239\\183\\144" "'" name)))
  ([ns name] (keyword (str* ns "/" name))))

(extend-type string
  IEquiv
  (-equiv [s o] (identical? s o))
  
  IHash
  (-hash [o] (string/hashCode o))

  ISeqable
  (-seq [string] (prim-seq string 0))

  ASeq
  ISeq
  (-first [s] (string/sub s 1 1))
  (-rest [s] (string/sub s 2))

  INext
  (-next [s] (let [a (string/sub s 2)]
               (if (identical? a "") nil a)))

  ICounted
  (-count [s] (alength s))

  IIndexed
  (-nth
    [string n]
    (if (< n (-count string)) (string/sub string (+ n  1) (+ n 1))))
  (-nth
    [string n not-found]
    (if (< n (-count string)) (string/sub string (+ n 1) (+ n 1))
        not-found))

  ILookup
  (-lookup
    [string k]
    (-nth string k))
  (-lookup
    [string k not_found]
    (-nth string k not_found))

  IReduce
  (-reduce
    [string f]
    (ci-reduce string f))
  (-reduce
    [string f start]
    (ci-reduce string f start))

  IFn
  (-invoke
    ([this coll]
       (get coll (toString this))))
  
  (-invoke
    ([this coll not-found]
       (get coll (toString this) not-found))))

(defn to-array
  "Naive impl of to-array as a start."
  [s]
  (let [ary (array)]
    (loop [s s]
      (if (seq s)
        (do (table/insert ary (first s))
            (recur (next s)))
        ary))))


(defn- fix [q]
  (if (>= q 0)
    (math/floor q)
    (math/ceil q)))

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args.
  First cut.  Not lazy.  Needs to use emitted toApply."
  ([f args]
     (f (lua/unpack (to-array args))))
  ([f x args]
     (f x (lua/unpack (to-array args))))
  ([f x y args]
     (f x y (lua/unpack (to-array args))))
  ([f x y z args]
     (f x y z (lua/unpack (to-array args))))
  ([f a b c d & args]
     (let [arglist (cons a (cons b (cons c (cons d (spread args)))))]
       (f x y z (lua/unpack (to-array arglist))))))


;;; Vector
;;; DEPRECATED
;;; in favor of PersistentVector
(deftype Vector [meta array ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IWithMeta
  (-with-meta [coll meta] (Vector. meta array __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (let [count (alength array)]
      (when (> count 0)
        (aget array (dec count)))))
  (-pop [coll]
    (if (> (alength array) 0)
      (let [new-array (aclone array)]
        (. new-array (pop))
        (Vector. meta new-array nil))
      (throw (js/Error. "Can't pop empty vector"))))

  ICollection
  (-conj [coll o]
    (let [new-array (aclone array)]
      (.push new-array o)
      (Vector. meta new-array nil)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.Vector/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (when (> (alength array) 0)
      (let [vector-seq
             (fn vector-seq [i]
               (lazy-seq
                 (when (< i (alength array))
                   (cons (aget array i) (vector-seq (inc i))))))]
        (vector-seq 0))))

  ICounted
  (-count [coll] (alength array))

  IIndexed
  (-nth [coll n]
    (if (and (<= 0 n) (< n (alength array)))
      (aget array n)
      #_(throw (js/Error. (str "No item " n " in vector of length " (alength array))))))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n (alength array)))
      (aget array n)
      not-found))

  ILookup
  (-lookup [coll k] (-nth coll k nil))
  (-lookup [coll k not-found] (-nth coll k not-found))

  IAssociative
  (-assoc [coll k v]
    (let [new-array (aclone array)]
      (aset new-array k v)
      (Vector. meta new-array nil)))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce [v f]
    (ci-reduce array f))
  (-reduce [v f start]
    (ci-reduce array f start))

  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found)))


(deftype PersistentVector [meta cnt shift root tail ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IWithMeta
  (-with-meta [coll meta] (PersistentVector. meta cnt shift root tail __hash))

  IMeta
  (-meta [coll] meta)

  IStack
  (-peek [coll]
    (when (> cnt 0)
      (-nth coll (dec cnt))))
  (-pop [coll]
    (cond
     (zero? cnt) (throw (js/Error. "Can't pop empty vector"))
     (== 1 cnt) (-with-meta cljs.core.PersistentVector/EMPTY meta)
     (< 1 (- cnt (tail-off coll)))
      (PersistentVector. meta (dec cnt) shift root (table/slice tail 1 -2) nil)
      :else (let [new-tail (array-for coll (- cnt 2))
                  nr (pop-tail coll shift root)
                  new-root (if (nil? nr) cljs.core.PersistentVector/EMPTY_NODE nr)
                  cnt-1 (dec cnt)]
              (if (and (< 5 shift) (nil? (pv-aget new-root 1)))
                (PersistentVector. meta cnt-1 (- shift 5) (pv-aget new-root 0) new-tail nil)
                (PersistentVector. meta cnt-1 shift new-root new-tail nil)))))

  ICollection
  (-conj [coll o]
    (if (< (- cnt (tail-off coll)) 32)
      (let [new-tail (aclone tail)]
        (table/insert new-tail o)
        (PersistentVector. meta (inc cnt) shift root new-tail nil))
      (let [root-overflow? (> (bit-shift-right-zero-fill cnt 5) (bit-shift-left 1 shift))
            new-shift (if root-overflow? (+ shift 5) shift)
            new-root (if root-overflow?
                       (let [n-r (pv-fresh-node nil)]
                           (pv-aset n-r 0 root)
                           (pv-aset n-r 1 (new-path nil shift (VectorNode. nil tail)))
                           n-r)
                       (push-tail coll shift root (VectorNode. nil tail)))]
        (PersistentVector. meta (inc cnt) new-shift new-root (array o) nil))))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.PersistentVector/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  ISeqable
  (-seq [coll]
    (if (zero? cnt)
      nil
      (chunked-seq coll 0 0)))

  ICounted
  (-count [coll] cnt)

  IIndexed
  (-nth [coll n]
    (aget (array-for coll n) (bit-and n 0x01f)))
  (-nth [coll n not-found]
    (if (and (<= 0 n) (< n cnt))
      (-nth coll n)
      not-found))

  ILookup
  (-lookup [coll k] (-nth coll k nil))
  (-lookup [coll k not-found] (-nth coll k not-found))

  IMapEntry
  (-key [coll]
    (-nth coll 0))
  (-val [coll]
    (-nth coll 1))

  IAssociative
  (-assoc [coll k v]
    (cond
       (and (<= 0 k) (< k cnt))
       (if (<= (tail-off coll) k)
         (let [new-tail (aclone tail)]
           (aset new-tail (bit-and k 0x01f) v)
           (PersistentVector. meta cnt shift root new-tail nil))
         (PersistentVector. meta cnt shift (do-assoc coll shift root k v) tail nil))
       (== k cnt) (-conj coll v)
       :else (throw (js/Error. (str "Index " k " out of bounds  [0," cnt "]")))))

  IVector
  (-assoc-n [coll n val] (-assoc coll n val))

  IReduce
  (-reduce [v f]
    (ci-reduce v f))
  (-reduce [v f start]
    (ci-reduce v f start))

  IKVReduce
  (-kv-reduce [v f init]
    (let [step-init (array 0 init)] ; [step 0 init init]
      (loop [i 0]
        (if (< i cnt)
          (let [arr (array-for v i)
                len (alength arr)]
            (let [init (loop [j 0 init (aget step-init 1)]
                         (if (< j len)
                           (let [init (f init (+ j i) (aget arr j))]
                             (if (reduced? init)
                               init
                               (recur (inc j) init)))
                           (do (aset step-init 0 len)
                               (aset step-init 1 init)
                               init)))]
              (if (reduced? init)
                @init
                (recur (+ i (aget step-init 0))))))
          (aget step-init 1)))))

  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientVector. cnt shift (tv-editable-root root) (tv-editable-tail tail)))

  IReversible
  (-rseq [coll]
    (if (pos? cnt)
      (RSeq. coll (dec cnt) nil)
      ())))

(set! cljs.core.PersistentVector/fromArray
      (fn [xs no-clone]
        (let [l (alength xs)
              xs (if (identical? no-clone true) xs (aclone xs))]
          (if (< l 32)
            (PersistentVector. nil l 5 cljs.core.PersistentVector/EMPTY_NODE xs nil)
            (let [node (table/slice xs 1 32)
                  v (PersistentVector. nil 32 5 cljs.core.PersistentVector/EMPTY_NODE node nil)]
             (loop [i 32 out (-as-transient v)]
               (if (< i l)
                 (recur (inc i) (conj! out (aget xs i)))
                 (persistent! out))))))))

(defn- tv-editable-tail [tl]
  (let [ret (make-array 32)]
    (array-copy tl 0 ret 0 (alength tl))
    ret))

(deftype ChunkBuffer [^:mutable buf ^:mutable len]
  ICounted
  (-count [_] len))


(defn chunk-append [cb o]
  (aset (.-buf cb) (.-len cb) o)
  (set! (.-len cb) (inc (.-len cb))))

(defn chunk [cb]
  (let [ret (ArrayChunk. (.-buf cb) 0 (.-len cb))]
    (set! (.-buf cb) nil)
    ret))


(defn- obj-map->hash-map [m k v]
  (let [ks  (.-keys m)
        len (alength ks)
        so  (.-strobj m)
        out (with-meta cljs.core.PersistentHashMap/EMPTY (meta m))]
    (loop [i   0
           out (transient out)]
      (if (< i len)
        (let [k (agetg ks i)]
          (recur (inc i) (assoc! out k (agetg so k))))
        (persistent! (assoc! out k v))))))


(defn- obj-clone [obj ks]
  (let [new-obj (js-obj)
        l (alength ks)]
    (loop [i 0]
      (when (< i l)
        (let [k (aget ks i)]
          (asetg new-obj k (agetg obj k))
          (recur (inc i)))))
    new-obj))

(deftype ObjMap [meta keys strobj update-count ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))
  
  IWithMeta
  (-with-meta [coll meta] (ObjMap. meta keys strobj update-count __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.ObjMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? (alength keys))
      (map #(vector % (agetg strobj %))
           (sort keys))))

  ICounted
  (-count [coll] (alength keys))

  ILookup
  (-lookup [coll k] (-lookup coll k nil))
  (-lookup [coll k not-found]
    (if (and ^boolean (lua-string? k)
             (not (nil? (scan-array 1 k keys))))
      (agetg strobj k)
      not-found))

  IAssociative
  (-assoc [coll k v]
    (if ^boolean (lua-string? k)
        (if (or (> update-count cljs.core.ObjMap/HASHMAP_THRESHOLD)
                (>= (alength keys) cljs.core.ObjMap/HASHMAP_THRESHOLD))
          (obj-map->hash-map coll k v)
          (if-not (nil? (scan-array 1 k keys))
            (let [new-strobj (obj-clone strobj keys)]
              (asetg new-strobj k v)
              (ObjMap. meta keys new-strobj (inc update-count) nil)) ; overwrite
            (let [new-strobj (obj-clone strobj keys) ; append
                  new-keys (aclone keys)]
              (asetg new-strobj k v)
              (table/insert new-keys k)
              (ObjMap. meta new-keys new-strobj (inc update-count) nil))))
        ;; non-string key. game over.
        (obj-map->hash-map coll k v)))
  (-contains-key? [coll k]
    (if (and ^boolean (lua-string? k)
             (not (nil? (scan-array 1 k keys))))
      true
      false))

  IMap
  (-dissoc [coll k]
    (if (and ^boolean (lua-string? k)
             (not (nil? (scan-array 1 k keys))))
      (let [new-keys (aclone keys)
            new-strobj (obj-clone strobj keys)]
        (table/remove new-keys (inc (scan-array 1 k new-keys)))
        (js-delete new-strobj k)
        (ObjMap. meta new-keys new-strobj (inc update-count) nil))
      coll)) ; key not found, return coll unchanged

  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (transient (into (hash-map) coll))))

;;; HashMap
;;; NOT IMPLEMENTED !! DO NOT USE !!
(deftype HashMap [])

(defn- extend-object!
  "Takes a JavaScript object and a map of names to functions and
  attaches said functions as methods on the object.  Any references to
  JavaScript's implict this (via the this-as macro) will resolve to the
  object that the function is attached."
  [obj fn-map]
  (doseq [[key-name f] fn-map]
    (let [str-name (name key-name)]
      (asetg obj str-name f)))
  obj)

;;; PersistentArrayMap

(defn- array-map-index-of [m k]
  (let [arr (.-arr m)
        len (alength arr)]
    (loop [i 0]
      (cond
        (<= len i) -1
        (= (aget arr i) k) i
        :else (recur (+ i 2))))))


(deftype PersistentArrayMap [meta cnt arr ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IWithMeta
  (-with-meta [coll meta] (PersistentArrayMap. meta cnt arr __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj coll entry)))

  IEmptyableCollection
  (-empty [coll] (-with-meta cljs.core.PersistentArrayMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? cnt)
      (let [len (alength arr)
            array-map-seq
            (fn array-map-seq [i]
              (lazy-seq
               (when (< i len)
                 (cons [(aget arr i) (aget arr (inc i))]
                       (array-map-seq (+ i 2))))))]
        (array-map-seq 0))))

  ICounted
  (-count [coll] cnt)

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (let [idx (array-map-index-of coll k)]
      (if (== idx -1)
        not-found
        (aget arr (inc idx)))))

  IAssociative
  (-assoc [coll k v]
    (let [idx (array-map-index-of coll k)]
      (cond
        (== idx -1)
        (if (< cnt cljs.core.PersistentArrayMap/HASHMAP_THRESHOLD)
          (PersistentArrayMap. meta
                               (inc cnt)
                               (doto (aclone arr)
                                 (table/insert k)
                                 (table/insert v))
                               nil)
          (persistent!
           (assoc!
            (transient (into cljs.core.PersistentHashMap/EMPTY coll))
            k v)))

        (identical? v (aget arr (inc idx)))
        coll

        :else
        (PersistentArrayMap. meta
                             cnt
                             (doto (aclone arr)
                               (aset (inc idx) v))
                             nil))))

  (-contains-key? [coll k]
    (not (== (array-map-index-of coll k) -1)))

  IMap
  (-dissoc [coll k]
    (let [idx (array-map-index-of coll k)]
      (if (>= idx 0)
        (let [len     (alength arr)
              new-len (- len 2)]
          (if (zero? new-len)
            (-empty coll)
            (let [new-arr (make-array new-len)]
              (loop [s 0 d 0]
                (cond
                  (>= s len) (PersistentArrayMap. meta (dec cnt) new-arr nil)
                  (= k (aget arr s)) (recur (+ s 2) d)
                  :else (do (aset new-arr d (aget arr s))
                            (aset new-arr (inc d) (aget arr (inc s)))
                            (recur (+ s 2) (+ d 2))))))))
        coll)))

  IKVReduce
  (-kv-reduce [coll f init]
    (let [len (alength arr)]
      (loop [i 0 init init]
        (if (< i len)
          (let [init (f init (aget arr i) (aget arr (inc i)))]
            (if (reduced? init)
              @init
              (recur (+ i 2) init)))))))

  IFn
  (-invoke [coll k]
    (-lookup coll k))

  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientArrayMap. (js-obj) (alength arr) (aclone arr))))



(deftype TransientArrayMap [^:mutable editable?
                            ^:mutable len
                            arr]
  ICounted
  (-count [tcoll]
    (if editable?
      (quot len 2)
      (throw (js/Error. "count after persistent!"))))

  ILookup
  (-lookup [tcoll k]
    (-lookup tcoll k nil))

  (-lookup [tcoll k not-found]
    (if editable?
      (let [idx (array-map-index-of tcoll k)]
        (if (== idx -1)
          not-found
          (aget arr (inc idx))))
      (throw (js/Error. "lookup after persistent!"))))

  ITransientCollection
  (-conj! [tcoll o]
    (if editable?
      (if (satisfies? IMapEntry o)
        (-assoc! tcoll (key o) (val o))
        (loop [es (seq o) tcoll tcoll]
          (if-let [e (first es)]
            (recur (next es)
                   (-assoc! tcoll (key e) (val e)))
            tcoll)))
      (throw (js/Error. "conj! after persistent!"))))

  (-persistent! [tcoll]
    (if editable?
      (do (set! editable? false)
          (PersistentArrayMap. nil (quot len 2) arr nil))
      (throw (js/Error. "persistent! called twice"))))

  ITransientAssociative
  (-assoc! [tcoll key val]
    (if editable?
      (let [idx (array-map-index-of tcoll key)]
        (if (== idx -1)
          (if (<= (+ len 2) (* 2 cljs.core.PersistentArrayMap/HASHMAP_THRESHOLD))
            (do (set! len (+ len 2))
                (table/insert arr key)
                (table/insert arr val)
                tcoll)
            (assoc! (array->transient-hash-map len arr) key val))
          (if (identical? val (aget arr (inc idx)))
            tcoll
            (do (aset arr (inc idx) val)
                tcoll))))
      (throw (js/Error. "assoc! after persistent!"))))

  ITransientMap
  (-dissoc! [tcoll key]
    (if editable?
      (let [idx (array-map-index-of tcoll key)]
        (when (>= idx 0)
          (aset arr idx (aget arr (- len 2)))
          (aset arr (inc idx) (aget arr (dec len)))
          (table/remove arr)
          (table/remove arr)
          (set! len (- len 2)))
        tcoll)
      (throw (js/Error. "dissoc! after persistent!")))))

(defn ^boolean key-test [key other]
  (if ^boolean (lua-string? key)
    (identical? key other)
    (= key other)))

(defn- remove-pair [arr i]
  (let [new-arr (make-array (- (alength arr) 2))]
    (array-copy arr 0 new-arr 0 (* 2 i))
    (array-copy arr (* 2 (inc i)) new-arr (* 2 i) (- (alength new-arr) (* 2 i)))
    new-arr))

(defn- edit-and-set
  ([inode edit i a]
     (let [editable (ensure-editable inode edit)]
       (aset (.-arr editable) i a)
       editable))
  ([inode edit i a j b]
     (let [editable (ensure-editable inode edit)]
       (aset (.-arr editable) i a)
       (aset (.-arr editable) j b)
       editable)))

(defn- inode-kv-reduce [arr f init]
  (let [len (alength arr)]
    (loop [i 0 init init]
      (if (< i len)
        (let [init (let [k (aget arr i)]
                     (if-not (nil? k)
                       (f init k (aget arr (inc i)))
                       (let [node (aget arr (inc i))]
                         (if-not (nil? node)
                           (.kv-reduce node f init)
                           init))))]
          (if (reduced? init)
            @init
            (recur (+ i 2) init)))
        init))))


(deftype BitmapIndexedNode [edit ^:mutable bitmap ^:mutable arr]
  INode
  (inode-assoc [inode shift hash key val added-leaf?]
    (let [bit (bitpos hash shift)
          idx (bitmap-indexed-node-index bitmap bit)]
      (if (zero? (bit-and bitmap bit))
        (let [n (bit-count bitmap)]
          (if (>= n 16)
            (let [nodes (make-array 32)
                  jdx   (mask hash shift)]
              (aset nodes jdx (inode-assoc cljs.core.BitmapIndexedNode/EMPTY (+ shift 5) hash key val added-leaf?))
              (loop [i 0 j 0]
                (if (< i 32)
                  (if (zero? (bit-and (bit-shift-right-zero-fill bitmap i) 1))
                    (recur (inc i) j)
                    (do (aset nodes i
                              (if-not (nil? (aget arr j))
                                (inode-assoc cljs.core.BitmapIndexedNode/EMPTY
                                              (+ shift 5) (cljs.core/hash (aget arr j)) (aget arr j) (aget arr (inc j)) added-leaf?)
                                (aget arr (inc j))))
                        (recur (inc i) (+ j 2))))))
              (ArrayNode. nil (inc n) nodes))
            (let [new-arr (make-array (* 2 (inc n)))]
              (array-copy arr 0 new-arr 0 (* 2 idx))
              (aset new-arr (* 2 idx) key)
              (aset new-arr (inc (* 2 idx)) val)
              (array-copy arr (* 2 idx) new-arr (* 2 (inc idx)) (* 2 (- n idx)))
              (set! (.-val added-leaf?) true)
              (BitmapIndexedNode. nil (bit-or bitmap bit) new-arr))))
        (let [key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (inode-assoc val-or-node (+ shift 5) hash key val added-leaf?)]
                  (if (identical? n val-or-node)
                    inode
                    (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) n))))

                (key-test key key-or-nil)
                (if (identical? val val-or-node)
                  inode
                  (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) val)))

                :else
                (do (set! (.-val added-leaf?) true)
                    (BitmapIndexedNode. nil bitmap
                                        (clone-and-set arr (* 2 idx) nil (inc (* 2 idx))
                                                       (create-node (+ shift 5) key-or-nil val-or-node hash key val)))))))))

  (inode-without [inode shift hash key]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        inode
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (inode-without val-or-node (+ shift 5) hash key)]
                  (cond (identical? n val-or-node) inode
                        (not (nil? n)) (BitmapIndexedNode. nil bitmap (clone-and-set arr (inc (* 2 idx)) n))
                        (== bitmap bit) nil
                        :else (BitmapIndexedNode. nil (bit-xor bitmap bit) (remove-pair arr idx))))
                (key-test key key-or-nil)
                (BitmapIndexedNode. nil (bit-xor bitmap bit) (remove-pair arr idx))
                :else inode)))))

  (inode-lookup [inode shift hash key not-found]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        not-found
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)  (inode-lookup val-or-node (+ shift 5) hash key not-found)
                (key-test key key-or-nil) val-or-node
                :else not-found)))))

  (inode-find [inode shift hash key not-found]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        not-found
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil) (inode-find val-or-node (+ shift 5) hash key not-found)
                (key-test key key-or-nil)          [key-or-nil val-or-node]
                :else not-found)))))

  (inode-seq [inode]
    (create-inode-seq arr))

  (ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (let [n       (bit-count bitmap)
            new-arr (make-array (if (neg? n) 4 (* 2 (inc n))))]
        (array-copy arr 0 new-arr 0 (* 2 n))
        (BitmapIndexedNode. e bitmap new-arr))))

  (inode-assoc! [inode edit shift hash key val added-leaf?]
    (let [bit (bitpos hash shift)
          idx (bitmap-indexed-node-index bitmap bit)]
      (if (zero? (bit-and bitmap bit))
        (let [n (bit-count bitmap)]
          (cond
            (< (* 2 n) (alength arr))
            (let [editable (ensure-editable inode edit)
                  earr     (.-arr editable)]
              (set! (.-val added-leaf?) true)
              (array-copy-downward earr (* 2 idx)
                                   earr (* 2 (inc idx))
                                   (* 2 (- n idx)))
              (aset earr (* 2 idx) key)
              (aset earr (inc (* 2 idx)) val)
              (set! (.-bitmap editable) (bit-or (.-bitmap editable) bit))
              editable)

            (>= n 16)
            (let [nodes (make-array 32)
                  jdx   (mask hash shift)]
              (aset nodes jdx (inode-assoc! cljs.core.BitmapIndexedNode/EMPTY edit (+ shift 5) hash key val added-leaf?))
              (loop [i 0 j 0]
                (if (< i 32)
                  (if (zero? (bit-and (bit-shift-right-zero-fill bitmap i) 1))
                    (recur (inc i) j)
                    (do (aset nodes i
                              (if-not (nil? (aget arr j))
                                (inode-assoc! cljs.core.BitmapIndexedNode/EMPTY
                                               edit (+ shift 5) (cljs.core/hash (aget arr j)) (aget arr j) (aget arr (inc j)) added-leaf?)
                                (aget arr (inc j))))
                        (recur (inc i) (+ j 2))))))
              (ArrayNode. edit (inc n) nodes))

            :else
            (let [new-arr (make-array (* 2 (+ n 4)))]
              (array-copy arr 0 new-arr 0 (* 2 idx))
              (aset new-arr (* 2 idx) key)
              (aset new-arr (inc (* 2 idx)) val)
              (array-copy arr (* 2 idx) new-arr (* 2 (inc idx)) (* 2 (- n idx)))
              (set! (.-val added-leaf?) true)
              (let [editable (ensure-editable inode edit)]
                (set! (.-arr editable) new-arr)
                (set! (.-bitmap editable) (bit-or (.-bitmap editable) bit))
                editable))))
        (let [key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (inode-assoc! val-or-node edit (+ shift 5) hash key val added-leaf?)]
                  (if (identical? n val-or-node)
                    inode
                    (edit-and-set inode edit (inc (* 2 idx)) n)))

                (key-test key key-or-nil)
                (if (identical? val val-or-node)
                  inode
                  (edit-and-set inode edit (inc (* 2 idx)) val))

                :else
                (do (set! (.-val added-leaf?) true)
                    (edit-and-set inode edit (* 2 idx) nil (inc (* 2 idx))
                                  (create-node edit (+ shift 5) key-or-nil val-or-node hash key val))))))))

  (inode-without! [inode edit shift hash key removed-leaf?]
    (let [bit (bitpos hash shift)]
      (if (zero? (bit-and bitmap bit))
        inode
        (let [idx         (bitmap-indexed-node-index bitmap bit)
              key-or-nil  (aget arr (* 2 idx))
              val-or-node (aget arr (inc (* 2 idx)))]
          (cond (nil? key-or-nil)
                (let [n (inode-without! val-or-node edit (+ shift 5) hash key removed-leaf?)]
                  (cond (identical? n val-or-node) inode
                        (not (nil? n)) (edit-and-set inode edit (inc (* 2 idx)) n)
                        (== bitmap bit) nil
                        :else (edit-and-remove-pair inode edit bit idx)))
                (key-test key key-or-nil)
                (do (aset removed-leaf? 0 true)
                    (edit-and-remove-pair inode edit bit idx))
                :else inode)))))

  (kv-reduce [inode f init]
    (inode-kv-reduce arr f init))

  IBitmapIndexedNode
  (edit-and-remove-pair [inode e bit i]
    (if (== bitmap bit)
      nil
      (let [editable (ensure-editable inode e)
            earr     (.-arr editable)
            len      (alength earr)]
        (set! (.-bitmap editable) (bit-xor bit (.-bitmap editable)))
        (array-copy earr (* 2 (inc i))
                    earr (* 2 i)
                    (- len (* 2 (inc i))))
        (aset earr (- len 2) nil)
        (aset earr (dec len) nil)
        editable))))


(deftype ArrayNode [edit ^:mutable cnt ^:mutable arr]
  INode
  (inode-assoc [inode shift hash key val added-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        (ArrayNode. nil (inc cnt) (clone-and-set arr idx (inode-assoc cljs.core.BitmapIndexedNode/EMPTY (+ shift 5) hash key val added-leaf?)))
        (let [n (inode-assoc node (+ shift 5) hash key val added-leaf?)]
          (if (identical? n node)
            inode
            (ArrayNode. nil cnt (clone-and-set arr idx n)))))))

  (inode-without [inode shift hash key]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (let [n (inode-without node (+ shift 5) hash key)]
          (cond
            (identical? n node)
            inode

            (nil? n)
            (if (<= cnt 8)
              (pack-array-node inode nil idx)
              (ArrayNode. nil (dec cnt) (clone-and-set arr idx n)))

            :else
            (ArrayNode. nil cnt (clone-and-set arr idx n))))
        inode)))

  (inode-lookup [inode shift hash key not-found]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (inode-lookup node (+ shift 5) hash key not-found)
        not-found)))

  (inode-find [inode shift hash key not-found]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if-not (nil? node)
        (inode-find node (+ shift 5) hash key not-found)
        not-found)))

  (inode-seq [inode]
    (create-array-node-seq arr))

  (ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (ArrayNode. e cnt (aclone arr))))

  (inode-assoc! [inode edit shift hash key val added-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        (let [editable (edit-and-set inode edit idx (inode-assoc! cljs.core.BitmapIndexedNode/EMPTY edit (+ shift 5) hash key val added-leaf?))]
          (set! (.-cnt editable) (inc (.-cnt editable)))
          editable)
        (let [n (inode-assoc! node edit (+ shift 5) hash key val added-leaf?)]
          (if (identical? n node)
            inode
            (edit-and-set inode edit idx n))))))

  (inode-without! [inode edit shift hash key removed-leaf?]
    (let [idx  (mask hash shift)
          node (aget arr idx)]
      (if (nil? node)
        inode
        (let [n (inode-without! node edit (+ shift 5) hash key removed-leaf?)]
          (cond
            (identical? n node)
            inode

            (nil? n)
            (if (<= cnt 8)
              (pack-array-node inode edit idx)
              (let [editable (edit-and-set inode edit idx n)]
                (set! (.-cnt editable) (dec (.-cnt editable)))
                editable))

            :else
            (edit-and-set inode edit idx n))))))

  (kv-reduce [inode f init]
    (let [len (alength arr)]           ; actually 32
      (loop [i 0 init init]
        (if (< i len)
          (let [node (aget arr i)]
            (if-not (nil? node)
              (let [init (kv-reduce node f init)]
                (if (reduced? init)
                  @init
                  (recur (inc i) init)))))
          init)))))


(deftype HashCollisionNode [edit
                            ^:mutable collision-hash
                            ^:mutable cnt
                            ^:mutable arr]
  INode
  (inode-assoc [inode shift hash key val added-leaf?]
    (if (== hash collision-hash)
      (let [idx (hash-collision-node-find-index arr cnt key)]
        (if (== idx -1)
          (let [len (alength arr)
                new-arr (make-array (+ len 2))]
            (array-copy arr 0 new-arr 0 len)
            (aset new-arr len key)
            (aset new-arr (inc len) val)
            (set! (.-val added-leaf?) true)
            (HashCollisionNode. nil collision-hash (inc cnt) new-arr))
          (if (= (aget arr idx) val)
            inode
            (HashCollisionNode. nil collision-hash cnt (clone-and-set arr (inc idx) val)))))
      (inode-assoc (BitmapIndexedNode. nil (bitpos collision-hash shift) (array nil inode))
                    shift hash key val added-leaf?)))

  (inode-without [inode shift hash key]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (== idx -1) inode
            (== cnt 1)  nil
            :else (HashCollisionNode. nil collision-hash (dec cnt) (remove-pair arr (quot idx 2))))))

  (inode-lookup [inode shift hash key not-found]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (< idx 0)              not-found
            (key-test key (aget arr idx)) (aget arr (inc idx))
            :else                  not-found)))

  (inode-find [inode shift hash key not-found]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (cond (< idx 0)              not-found
            (key-test key (aget arr idx)) [(aget arr idx) (aget arr (inc idx))]
            :else                  not-found)))

  (inode-seq [inode]
    (create-inode-seq arr))

  (ensure-editable [inode e]
    (if (identical? e edit)
      inode
      (let [new-arr (make-array (* 2 (inc cnt)))]
        (array-copy arr 0 new-arr 0 (* 2 cnt))
        (HashCollisionNode. e collision-hash cnt new-arr))))

  (inode-assoc! [inode edit shift hash key val added-leaf?]
    (if (== hash collision-hash)
      (let [idx (hash-collision-node-find-index arr cnt key)]
        (if (== idx -1)
          (if (> (alength arr) (* 2 cnt))
            (let [editable (edit-and-set inode edit (* 2 cnt) key (inc (* 2 cnt)) val)]
              (set! (.-val added-leaf?) true)
              (set! (.-cnt editable) (inc (.-cnt editable)))
              editable)
            (let [len     (alength arr)
                  new-arr (make-array (+ len 2))]
              (array-copy arr 0 new-arr 0 len)
              (aset new-arr len key)
              (aset new-arr (inc len) val)
              (set! (.-val added-leaf?) true)
              (ensure-editable-array inode edit (inc cnt) new-arr)))
          (if (identical? (aget arr (inc idx)) val)
            inode
            (edit-and-set inode edit (inc idx) val))))
      (inode-assoc! (BitmapIndexedNode. edit (bitpos collision-hash shift) (array nil inode nil nil))
                     edit shift hash key val added-leaf?)))

  (inode-without! [inode edit shift hash key removed-leaf?]
    (let [idx (hash-collision-node-find-index arr cnt key)]
      (if (== idx -1)
        inode
        (do (aset removed-leaf? 0 true)
            (if (== cnt 1)
              nil
              (let [editable (ensure-editable inode edit)
                    earr     (.-arr editable)]
                (aset earr idx (aget earr (- (* 2 cnt) 2)))
                (aset earr (inc idx) (aget earr (dec (* 2 cnt))))
                (aset earr (dec (* 2 cnt)) nil)
                (aset earr (- (* 2 cnt) 2) nil)
                (set! (.-cnt editable) (dec (.-cnt editable)))
                editable))))))

  (kv-reduce [inode f init]
    (inode-kv-reduce arr f init))

  IHashCollisionNode
  (ensure-editable-array [inode e count array]
    (if (identical? e edit)
      (do (set! arr array)
          (set! cnt count)
          inode)
      (HashCollisionNode. edit collision-hash count array))))


(defn- create-inode-seq
  ([nodes]
     (create-inode-seq nodes 0 nil))
  ([nodes i s]
     (if (nil? s)
       (let [len (alength nodes)]
         (loop [j i]
           (if (< j len)
             (if-not (nil? (aget nodes j))
               (NodeSeq. nil nodes j nil nil)
               (if-let [node (aget nodes (inc j))]
                 (if-let [node-seq (inode-seq node)]
                   (NodeSeq. nil nodes (+ j 2) node-seq nil)
                   (recur (+ j 2)))
                 (recur (+ j 2)))))))
       (NodeSeq. nil nodes i s nil))))

(defn- create-array-node-seq
  ([nodes] (create-array-node-seq nil nodes 0 nil))
  ([meta nodes i s]
     (if (nil? s)
       (let [len (alength nodes)]
         (loop [j i]
           (if (< j len)
             (if-let [nj (aget nodes j)]
               (if-let [ns (inode-seq nj)]
                 (ArrayNodeSeq. meta nodes (inc j) ns nil)
                 (recur (inc j)))
               (recur (inc j))))))
       (ArrayNodeSeq. meta nodes i s nil))))


(deftype PersistentHashMap [meta cnt root ^boolean has-nil? nil-val ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IWithMeta
  (-with-meta [coll meta] (PersistentHashMap. meta cnt root has-nil? nil-val __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj coll entry)))

  IEmptyableCollection
  (-empty [coll] (-with-meta cljs.core.PersistentHashMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ISeqable
  (-seq [coll]
    (when (pos? cnt)
      (let [s (if-not (nil? root) (inode-seq root))]
        (if has-nil?
          (cons [nil nil-val] s)
          s))))

  ICounted
  (-count [coll] cnt)

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (cond (nil? k)    (if has-nil?
                        nil-val
                        not-found)
          (nil? root) not-found
          :else       (inode-lookup root 0 (hash k) k not-found)))

  IAssociative
  (-assoc [coll k v]
    (if (nil? k)
      (if (and has-nil? (identical? v nil-val))
        coll
        (PersistentHashMap. meta (if has-nil? cnt (inc cnt)) root true v nil))
      (let [added-leaf? (Box. false)
            new-root    (-> (if (nil? root)
                              cljs.core.BitmapIndexedNode/EMPTY
                              root)
                            (inode-assoc 0 (hash k) k v added-leaf?))]
        (if (identical? new-root root)
          coll
          (PersistentHashMap. meta (if ^boolean (.-val added-leaf?) (inc cnt) cnt) new-root has-nil? nil-val nil)))))

  (-contains-key? [coll k]
    (cond (nil? k)    has-nil?
          (nil? root) false
          :else       (not (identical? (inode-lookup root 0 (hash k) k lookup-sentinel)
                                       lookup-sentinel))))

  IMap
  (-dissoc [coll k]
    (cond (nil? k)    (if has-nil?
                        (PersistentHashMap. meta (dec cnt) root false nil nil)
                        coll)
          (nil? root) coll
          :else
          (let [new-root (inode-without root 0 (hash k) k)]
            (if (identical? new-root root)
              coll
              (PersistentHashMap. meta (dec cnt) new-root has-nil? nil-val nil)))))

  IKVReduce
  (-kv-reduce [coll f init]
    (let [init (if has-nil? (f init nil nil-val) init)]
      (cond
        (reduced? init)          @init
        (not (nil? root)) (kv-reduce root f init)
        :else                    init)))

  IFn
  (-invoke [coll k]
    (-lookup coll k))

  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  IEditableCollection
  (-as-transient [coll]
    (TransientHashMap. (js-obj) root cnt has-nil? nil-val)))

(set! cljs.core.PersistentHashMap/EMPTY (PersistentHashMap. nil 0 nil false nil 0))

(set! cljs.core.PersistentHashMap/fromArrays
      (fn [ks vs]
        (let [len (alength ks)]
          (loop [i 0 out (transient cljs.core.PersistentHashMap/EMPTY)]
            (if (< i len)
              (recur (inc i) (assoc! out (aget ks i) (aget vs i)))
              (persistent! out))))))


(deftype TransientHashMap [^:mutable ^boolean edit
                           ^:mutable root
                           ^:mutable count
                           ^:mutable ^boolean has-nil?
                           ^:mutable nil-val]
  ITransientHashMap
  (tconj! [tcoll o]
    (if edit
      (if (satisfies? IMapEntry o)
        (tassoc! tcoll (key o) (val o))
        (loop [es (seq o) tcoll tcoll]
          (if-let [e (first es)]
            (recur (next es)
                   (tassoc! tcoll (key e) (val e)))
            tcoll)))
      (throw (js/Error. "conj! after persistent"))))

  (tassoc! [tcoll k v]
    (if edit
      (if (nil? k)
        (do (if (identical? nil-val v)
              nil
              (set! nil-val v))
            (if has-nil?
              nil
              (do (set! count (inc count))
                  (set! has-nil? true)))
            tcoll)
        (let [added-leaf? (Box. false)
              node        (-> (if (nil? root)
                                cljs.core.BitmapIndexedNode/EMPTY
                                root)
                              (inode-assoc! edit 0 (hash k) k v added-leaf?))]
          (if (identical? node root)
            nil
            (set! root node))
          (if ^boolean (.-val added-leaf?)
            (set! count (inc count)))
          tcoll))
      (throw (js/Error. "assoc! after persistent!"))))

  (twithout! [tcoll k]
    (if edit
      (if (nil? k)
        (if has-nil?
          (do (set! has-nil? false)
              (set! nil-val nil)
              (set! count (dec count))
              tcoll)
          tcoll)
        (if (nil? root)
          tcoll
          (let [removed-leaf? (Box. false)
                node (inode-without! root edit 0 (hash k) k removed-leaf?)]
            (if (identical? node root)
              nil
              (set! root node))
            (if (aget removed-leaf? 0)
              (set! count (dec count)))
            tcoll)))
      (throw (js/Error. "dissoc! after persistent!"))))

  (tpersistent! [tcoll]
    (if edit
      (do (set! edit nil)
          (PersistentHashMap. nil count root has-nil? nil-val nil))
      (throw (js/Error. "persistent! called twice"))))

  ICounted
  (-count [coll]
    (if edit
      count
      (throw (js/Error. "count after persistent!"))))

  ILookup
  (-lookup [tcoll k]
    (if (nil? k)
      (if has-nil?
        nil-val)
      (if (nil? root)
        nil
        (inode-lookup root 0 (hash k) k))))

  (-lookup [tcoll k not-found]
    (if (nil? k)
      (if has-nil?
        nil-val
        not-found)
      (if (nil? root)
        not-found
        (inode-lookup root 0 (hash k) k not-found))))

  ITransientCollection
  (-conj! [tcoll val] (tconj! tcoll val))

  (-persistent! [tcoll] (tpersistent! tcoll))

  ITransientAssociative
  (-assoc! [tcoll key val] (tassoc! tcoll key val))

  ITransientMap
  (-dissoc! [tcoll key] (twithout! tcoll key)))


(defn- balance-left [key val ins right]
  (if (instance? RedNode ins)
    (cond
      (instance? RedNode (.-left ins))
      (RedNode. (.-key ins) (.-val ins)
              (blacken (.-left ins))
              (BlackNode. key val (.-right ins) right nil)
              nil)

      (instance? RedNode (.-right ins))
      (RedNode. (.. ins -right -key) (.. ins -right -val)
                (BlackNode. (.-key ins) (.-val ins)
                            (.-left ins)
                            (.. ins -right -left)
                            nil)
                (BlackNode. key val
                            (.. ins -right -right)
                            right
                            nil)
                nil)

      :else
      (BlackNode. key val ins right nil))
    (BlackNode. key val ins right nil)))

(defn- balance-right [key val left ins]
  (if (instance? RedNode ins)
    (cond
      (instance? RedNode (.-right ins))
      (RedNode. (.-key ins) (.-val ins)
                (BlackNode. key val left (.-left ins) nil)
                (blacken (.-right ins))
                nil)

      (instance? RedNode (.-left ins))
      (RedNode. (.. ins -left -key) (.. ins -left -val)
                (BlackNode. key val left (.. ins -left -left) nil)
                (BlackNode. (.-key ins) (.-val ins)
                            (.. ins -left -right)
                            (.-right ins)
                            nil)
                nil)

      :else
      (BlackNode. key val left ins nil))
    (BlackNode. key val left ins nil)))

(defn- balance-left-del [key val del right]
  (cond
    (instance? RedNode del)
    (RedNode. key val (blacken del) right nil)

    (instance? BlackNode right)
    (balance-right key val del (redden right))

    (and (instance? RedNode right) (instance? BlackNode (.-left right)))
    (RedNode. (.. right -left -key) (.. right -left -val)
              (BlackNode. key val del (.. right -left -left) nil)
              (balance-right (.-key right) (.-val right)
                             (.. right -left -right)
                             (redden (.-right right)))
              nil)

    :else
    (throw (js/Error. "red-black tree invariant violation"))))

(defn- balance-right-del [key val left del]
  (cond
    (instance? RedNode del)
    (RedNode. key val left (blacken del) nil)

    (instance? BlackNode left)
    (balance-left key val (redden left) del)

    (and (instance? RedNode left) (instance? BlackNode (.-right left)))
    (RedNode. (.. left -right -key) (.. left -right -val)
              (balance-left (.-key left) (.-val left)
                            (redden (.-left left))
                            (.. left -right -left))
              (BlackNode. key val (.. left -right -right) del nil)
              nil)

    :else
    (throw (js/Error. "red-black tree invariant violation"))))

(deftype BlackNode [key val left right ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IRBNode
  (add-left [node ins]
    (balance-left ins node))

  (add-right [node ins]
    (balance-right ins node))

  (remove-left [node del]
    (balance-left-del key val del right))

  (remove-right [node del]
    (balance-right-del key val left del))

  (blacken [node] node)

  (redden [node] (RedNode. key val left right nil))

  (balance-left [node parent]
    (BlackNode. (.-key parent) (.-val parent) node (.-right parent) nil))

  (balance-right [node parent]
    (BlackNode. (.-key parent) (.-val parent) (.-left parent) node nil))

  (nreplace [node key val left right]
    (BlackNode. key val left right nil))

  (kv-reduce [node f init]
    (tree-map-kv-reduce node f init))

  IMapEntry
  (-key [node] key)
  (-val [node] val)

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IMeta
  (-meta [node] nil)

  IWithMeta
  (-with-meta [node meta]
    (with-meta [key val] meta))

  IStack
  (-peek [node] val)

  (-pop [node] [key])

  ICollection
  (-conj [node o] [key val o])

  IEmptyableCollection
  (-empty [node] [])

  ISequential
  ISeqable
  (-seq [node] (list key val))

  ICounted
  (-count [node] 2)

  IIndexed
  (-nth [node n]
    (cond (== n 0) key
          (== n 1) val
          :else    nil))

  (-nth [node n not-found]
    (cond (== n 0) key
          (== n 1) val
          :else    not-found))

  ILookup
  (-lookup [node k] (-nth node k nil))
  (-lookup [node k not-found] (-nth node k not-found))

  IAssociative
  (-assoc [node k v]
    (assoc [key val] k v))

  IVector
  (-assoc-n [node n v]
    (-assoc-n [key val] n v))

  IReduce
  (-reduce [node f]
    (ci-reduce node f))

  (-reduce [node f start]
    (ci-reduce node f start))

  IFn
  (-invoke [node k]
    (-lookup node k))

  (-invoke [node k not-found]
    (-lookup node k not-found)))


(deftype RedNode [key val left right ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IRBNode
  (add-left [node ins]
    (RedNode. key val ins right nil))

  (add-right [node ins]
    (RedNode. key val left ins nil))

  (remove-left [node del]
    (RedNode. key val del right nil))

  (remove-right [node del]
    (RedNode. key val left del nil))

  (blacken [node]
    (BlackNode. key val left right nil))

  (redden [node]
    (throw (js/Error. "red-black tree invariant violation")))

  (balance-left [node parent]
    (cond
      (instance? RedNode left)
      (RedNode. key val
                (blacken left)
                (BlackNode. (.-key parent) (.-val parent) right (.-right parent) nil)
                nil)

      (instance? RedNode right)
      (RedNode. (.-key right) (.-val right)
                (BlackNode. key val left (.-left right) nil)
                (BlackNode. (.-key parent) (.-val parent)
                            (.-right right)
                            (.-right parent)
                            nil)
                nil)

      :else
      (BlackNode. (.-key parent) (.-val parent) node (.-right parent) nil)))

  (balance-right [node parent]
    (cond
      (instance? RedNode right)
      (RedNode. key val
                (BlackNode. (.-key parent) (.-val parent)
                            (.-left parent)
                            left
                            nil)
                (blacken right)
                nil)

      (instance? RedNode left)
      (RedNode. (.-key left) (.-val left)
                (BlackNode. (.-key parent) (.-val parent)
                            (.-left parent)
                            (.-left left)
                            nil)
                (BlackNode. key val (.-right left) right nil)
                nil)

      :else
      (BlackNode. (.-key parent) (.-val parent) (.-left parent) node nil)))

  (nreplace [node key val left right]
    (RedNode. key val left right nil))

  (kv-reduce [node f init]
    (tree-map-kv-reduce node f init))

  IMapEntry
  (-key [node] key)
  (-val [node] val)

  IHash
  (-hash [coll] (caching-hash coll hash-coll __hash))

  IEquiv
  (-equiv [coll other] (equiv-sequential coll other))

  IMeta
  (-meta [node] nil)

  IWithMeta
  (-with-meta [node meta]
    (with-meta [key val] meta))

  IStack
  (-peek [node] val)

  (-pop [node] [key])

  ICollection
  (-conj [node o] [key val o])

  IEmptyableCollection
  (-empty [node] [])

  ISequential
  ISeqable
  (-seq [node] (list key val))

  ICounted
  (-count [node] 2)

  IIndexed
  (-nth [node n]
    (cond (== n 0) key
          (== n 1) val
          :else    nil))

  (-nth [node n not-found]
    (cond (== n 0) key
          (== n 1) val
          :else    not-found))

  ILookup
  (-lookup [node k] (-nth node k nil))
  (-lookup [node k not-found] (-nth node k not-found))

  IAssociative
  (-assoc [node k v]
    (assoc [key val] k v))

  IVector
  (-assoc-n [node n v]
    (-assoc-n [key val] n v))

  IReduce
  (-reduce [node f]
    (ci-reduce node f))

  (-reduce [node f start]
    (ci-reduce node f start))

  IFn
  (-invoke [node k]
    (-lookup node k))

  (-invoke [node k not-found]
    (-lookup node k not-found)))


(defn- tree-map-add [comp tree k v found]
  (if (nil? tree)
    (RedNode. k v nil nil nil)
    (let [c (comp k (.-key tree))]
      (cond
        (zero? c)
        (do (aset found 0 tree)
            nil)

        (neg? c)
        (let [ins (tree-map-add comp (.-left tree) k v found)]
          (if-not (nil? ins)
            (add-left tree ins)))

        :else
        (let [ins (tree-map-add comp (.-right tree) k v found)]
          (if-not (nil? ins)
            (add-right tree ins)))))))

(defn- tree-map-replace [comp tree k v]
  (let [tk (.-key tree)
        c  (comp k tk)]
    (cond (zero? c) (nreplace tree tk v (.-left tree) (.-right tree))
          (neg? c)  (nreplace tree tk (.-val tree) (tree-map-replace comp (.-left tree) k v) (.-right tree))
          :else     (nreplace tree tk (.-val tree) (.-left tree) (tree-map-replace comp (.-right tree) k v)))))


(deftype PersistentTreeMap [comp tree cnt meta ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))

  IPersistentTreeMap
  (entry-at [coll k]
    (loop [t tree]
      (if-not (nil? t)
        (let [c (comp k (.-key t))]
          (cond (zero? c) t
                (neg? c)  (recur (.-left t))
                :else     (recur (.-right t)))))))
  
  IWithMeta
  (-with-meta [coll meta] (PersistentTreeMap. comp tree cnt meta __hash))

  IMeta
  (-meta [coll] meta)

  ICollection
  (-conj [coll entry]
    (if (vector? entry)
      (-assoc coll (-nth entry 0) (-nth entry 1))
      (reduce -conj
              coll
              entry)))

  IEmptyableCollection
  (-empty [coll] (with-meta cljs.core.PersistentTreeMap/EMPTY meta))

  IEquiv
  (-equiv [coll other] (equiv-map coll other))

  IHash
  (-hash [coll] (caching-hash coll hash-imap __hash))

  ICounted
  (-count [coll] cnt)

  IKVReduce
  (-kv-reduce [coll f init]
    (if-not (nil? tree)
      (tree-map-kv-reduce tree f init)
      init))

  IFn
  (-invoke [coll k]
    (-lookup coll k))

  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  ISeqable
  (-seq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree true cnt)))

  IReversible
  (-rseq [coll]
    (if (pos? cnt)
      (create-tree-map-seq tree false cnt)))

  ILookup
  (-lookup [coll k]
    (-lookup coll k nil))

  (-lookup [coll k not-found]
    (let [n (entry-at coll k)]
      (if-not (nil? n)
        (.-val n)
        not-found)))

  IAssociative
  (-assoc [coll k v]
    (let [found (array nil)
          t     (tree-map-add comp tree k v found)]
      (if (nil? t)
        (let [found-node (nth found 0)]
          (if (= v (.-val found-node))
            coll
            (PersistentTreeMap. comp (tree-map-replace comp tree k v) cnt meta nil)))
        (PersistentTreeMap. comp (blacken t) (inc cnt) meta nil))))

  (-contains-key? [coll k]
    (not (nil? (entry-at coll k))))

  IMap
  (-dissoc [coll k]
    (let [found (array nil)
          t     (tree-map-remove comp tree k found)]
      (if (nil? t)
        (if (nil? (nth found 0))
          coll
          (PersistentTreeMap. comp nil 0 meta nil))
        (PersistentTreeMap. comp (blacken t) (dec cnt) meta nil))))

  ISorted
  (-sorted-seq [coll ascending?]
    (if (pos? cnt)
      (create-tree-map-seq tree ascending? cnt)))

  (-sorted-seq-from [coll k ascending?]
    (if (pos? cnt)
      (loop [stack nil t tree]
        (if-not (nil? t)
          (let [c (comp k (.-key t))]
            (cond
              (zero? c)  (PersistentTreeMapSeq. nil (conj stack t) ascending? -1 nil)
              ascending? (if (neg? c)
                           (recur (conj stack t) (.-left t))
                           (recur stack          (.-right t)))
              :else      (if (pos? c)
                           (recur (conj stack t) (.-right t))
                           (recur stack          (.-left t)))))
          (if (nil? stack)
            (PersistentTreeMapSeq. nil stack ascending? -1 nil))))))

  (-entry-key [coll entry] (key entry))

  (-comparator [coll] comp))

(defn obj-map
  "keyval => key val
  Returns a new object map with supplied mappings."
  [& keyvals]
  (let [ks  (array)
        obj (js-obj)]
    (loop [kvs (seq keyvals)]
      (if kvs
        (do (table/insert ks (first kvs))
            (asetg obj (first kvs) (second kvs))
            (recur (nnext kvs)))
        (cljs.core.ObjMap/fromObject ks obj)))))


(deftype Range [meta start end step ^:mutable __hash]
  Object
  (toString [this]
    (pr-str this))
  
  IWithMeta
  (-with-meta [rng meta] (Range. meta start end step __hash))

  IMeta
  (-meta [rng] meta)

  ISeqable
  (-seq [rng]
    (if (pos? step)
      (when (< start end)
        rng)
      (when (> start end)
        rng)))

  ISeq
  (-first [rng] start)
  (-rest [rng]
    (if-not (nil? (-seq rng))
      (Range. meta (+ start step) end step nil)
      ()))

  INext
  (-next [rng]
    (if (pos? step)
      (when (< (+ start step) end)
        (Range. meta (+ start step) end step nil))
      (when (> (+ start step) end)
        (Range. meta (+ start step) end step nil))))

  ICollection
  (-conj [rng o] (cons o rng))

  IEmptyableCollection
  (-empty [rng] (with-meta cljs.core.List/EMPTY meta))

  ISequential
  IEquiv
  (-equiv [rng other] (equiv-sequential rng other))

  IHash
  (-hash [rng] (caching-hash rng hash-coll __hash))

  ICounted
  (-count [rng]
    (if-not (-seq rng)
      0
      (math/ceil (/ (- end start) step))))

  IIndexed
  (-nth [rng n]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (zero? step))
        start
        (throw (js/Error. "Index out of bounds")))))
  (-nth [rng n not-found]
    (if (< n (-count rng))
      (+ start (* n step))
      (if (and (> start end) (zero? step))
        start
        not-found)))

  IReduce
  (-reduce [rng f] (ci-reduce rng f))
  (-reduce [rng f s] (ci-reduce rng f s)))


(defn regexp? [o] (throw "NIY"))

(defn re-matches
  "Returns the result of (re-find re s) if re fully matches s."
  [re s]
  (throw "NIY"))

(defn re-find
  "Returns the first regex match, if any, of s to re, using
  re.exec(s). Returns a vector, containing first the matching
  substring, then any capturing groups if the regular expression contains
  capturing groups."
  [re s]
  (throw "NIY"))

(defn re-seq
  "Returns a lazy sequence of successive matches of re in s."
  [re s]
  (throw "NIY"))

(defn re-pattern
  "Returns an instance of RegExp which has compiled the provided string."
  [s]
  (throw "NIY"))

(defn- pr-seq [obj opts]
  (cond
    (nil? obj) (list "nil")
    :else (concat
            (when (and (get opts :meta)
                       (satisfies? IMeta obj)
                       (meta obj))
              (concat ["^"] (pr-seq (meta obj) opts) [" "]))
            (cond
             ;; handle CLJS ctors
             (and (not (nil? obj))
                  ^boolean (.-cljs__lang__type obj))
             (.cljs__lang__ctorPrSeq obj obj)

             (satisfies? IPrintable obj) (-pr-seq obj opts)

             (regexp? obj) (list "#\"" (.-source obj) "\"")

             :else (list "#<" (str obj) ">")))))

(defn- pr-sb [objs opts]
  (let [first-obj (first objs)
        tbl (js-obj)]
    (doseq [obj objs]
      (when-not (identical? obj first-obj)
        (table/insert tbl " "))
      (doseq [string (pr-seq obj opts)]
        (table/insert tbl string)))
    (table/concat sb)))

(defn prn-str-with-opts
  "Same as pr-str-with-opts followed by (newline)"
  [objs opts]
  (let [sb (pr-sb objs opts)]
    (table/insert sb \newline)
    (str sb)))

(extend-protocol IPrintable
  boolean
  (-pr-seq [bool opts] (list (str bool)))

  number
  (-pr-seq [n opts] (list (str n)))

  Array
  (-pr-seq [a opts]
    (pr-sequential pr-seq "#<Array [" ", " "]>" opts a))

  string
  (-pr-seq [obj opts]
    (cond
     (keyword? obj)
     (list (str ":"
                (when-let [nspc (namespace obj)]
                  (str nspc "/"))
                (name obj)))
     (symbol? obj)
     (list (str (when-let [nspc (namespace obj)]
                  (str nspc "/"))
                (name obj)))
     :else (list (if (:readably opts)
                   (goog.string.quote obj)
                   obj))))

  LazySeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  IndexedSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  RSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  PersistentQueue
  (-pr-seq [coll opts] (pr-sequential pr-seq "#queue [" " " "]" opts (seq coll)))

  PersistentTreeMapSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  NodeSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  ArrayNodeSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  List
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  Cons
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  EmptyList
  (-pr-seq [coll opts] (list "()"))

  Vector
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  PersistentVector
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  ChunkedCons
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  ChunkedSeq
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll))

  Subvec
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  BlackNode
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  RedNode
  (-pr-seq [coll opts] (pr-sequential pr-seq "[" " " "]" opts coll))

  ObjMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  HashMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  PersistentArrayMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  PersistentHashMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  PersistentTreeMap
  (-pr-seq [coll opts]
    (let [pr-pair (fn [keyval] (pr-sequential pr-seq "" " " "" opts keyval))]
      (pr-sequential pr-pair "{" ", " "}" opts coll)))

  PersistentHashSet
  (-pr-seq [coll opts] (pr-sequential pr-seq "#{" " " "}" opts coll))

  PersistentTreeSet
  (-pr-seq [coll opts] (pr-sequential pr-seq "#{" " " "}" opts coll))

  Range
  (-pr-seq [coll opts] (pr-sequential pr-seq "(" " " ")" opts coll)))


