(ns main
  (:require [testns.dep :as dep]))

(println (dep/testfn 12 15))