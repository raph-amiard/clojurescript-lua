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

(extend-type table
  Object
  (toString [t] (builtins/array-to-string t))
  
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
           (.sort keys obj-map-compare-keys))))

  ICounted
  (-count [coll] (.-length keys))

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
              (aset new-strobj k v)
              (ObjMap. meta keys new-strobj (inc update-count) nil)) ; overwrite
            (let [new-strobj (obj-clone strobj keys) ; append
                  new-keys (aclone keys)]
              (aset new-strobj k v)
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
        (.splice new-keys (scan-array 1 k new-keys) 1)
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