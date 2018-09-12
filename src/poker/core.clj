(ns poker.core
  (:require [clojure.java.io :as io]
            [poker.poker :as poker]
            [poker.dev :as dev]))

(defn x-1 []
  (println "Hi"))

;; input-2 ["AC" "2D" "7D" "3C" "TC" "3S" "JS" "4H" "4D" "JD"]
(defn x-2 []
  (let [inputs (line-seq (io/reader (io/resource "input.txt")))
        input-1 (nth inputs 11)
        input-2 "AC 2D 7D 3C TC 3S JS 4H 4D JD"
        ]
    (dev/log input-1)
    (dev/log input-2)
    (poker/play-game input-2)))

(defn x-3 []
  (let [inputs (line-seq (io/reader (io/resource "input.txt")))]
    (frequencies (map poker/play-game inputs))))

(defn x-6 []
  (poker/same-value ["9C" "9D" "8D" "7C" "3C"]))

(defn x-7 []
  (poker/value-with-frequency 2 ["9C" "9D" "8D" "7C" "3C"]))

(defn x-8 []
  (poker/highest-card-value ["9C" "9D" "8D" "7C" "3C"]))

(defn x-9 []
  (poker/three-of-a-kind ["2C" "9C" "3S" "3C" "3C"]))

(defn x-10 []
  (poker/two-kind-with-frequency 2 ["3C" "3C" "2C" "9C" "9S"]))

(defn x11 []
  (poker/winner [1 14 7 6 5 4] [2 2 7 2 2]))