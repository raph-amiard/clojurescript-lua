(ns cljs.core)

(defprotocol Object (toString [x]))

(defprotocol IStringBuffer (append [x str]))

(deftype StringBuffer []
  Object
  (toString [x] (table/concat x))
  IStringBuffer
  (append [x str] (table/insert x str) x))

(defn pr-str [] "NIY")