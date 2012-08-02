;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.lua.config
  (:require [clojure.java.io :as io]
            [cljs.cljsloader :as cloader]))

(def config-map (atom {}))

(defn load-config []
  (let [user-dir (System/getProperty "user.home")
        sep java.io.File/separator
        user-config (try (io/reader (str user-dir sep ".cljslua" sep ".config.clj"))
                         (catch Exception e
                           (io/reader (io/resource ".config.clj"))))]
    (reset! config-map (first (cloader/make-forms-seq user-config)))))

(defn get [& ks]
  (get-in @config-map ks))
                                   