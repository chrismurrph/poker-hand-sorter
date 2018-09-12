(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.pprint :as pp]))

(defn reset []
  (refresh))