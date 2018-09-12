(ns poker.core
  (:gen-class)
  (:require [poker.poker :as poker]))

(defn -main []
  (let [lines (line-seq (java.io.BufferedReader. *in*))]
    (->> lines
         (map poker/play-game)
         frequencies
         poker/required-output-format)))

