(ns poker.poker
  (:require
    [clojure.string :as str]
    [poker.dev :as dev])
  (:refer-clojure :exclude [flush]))

;; value is "K", "10" etc
;; value is also known as 'kind'
;; Any kind/value has an ordinal
;; It is better to represent values as their ordinal equivalent, so "K" as 13, so we do
;; suit is "C" (clubs) "S" (spades) etc

(defn has-value-hof [card]
  (fn [hand]
    (->> hand
         (map (comp str first))
         dev/probe-off
         (some #(= % card)))))
(def has-ten? (has-value-hof "T"))
(def has-jack? (has-value-hof "J"))
(def has-queen? (has-value-hof "Q"))
(def has-king? (has-value-hof "K"))
(def has-ace? (has-value-hof "A"))

;;
;; Returned in the form [n1 n2 ...]
;; Where n1 is the highest frequency, n2 the 2nd highest etc
;; n1 + n2 ... always adds up to 5
;; Thus two pair would return [2 2 1]
;; selector determines whether the frequencies are for the card value or the card suit.
;;
(defn descending-frequencies [selector]
  (fn [hand]
    (->> hand
         (map (comp str selector))
         frequencies
         vals
         (sort-by -))))
(def same-suit (descending-frequencies second))
(def same-value (descending-frequencies first))

(def picture-value->ordinal
  {"A" 14
   "K" 13
   "Q" 12
   "J" 11
   "T" 10})

(defn value->ordinal [card]
  (assert (some? card))
  (or (picture-value->ordinal card) (Integer/parseInt card)))

(defn count-consecutive [hand]
  (->> hand
       (map (comp str first))
       sort
       (map value->ordinal)
       (partition 2 1)
       (drop-while (fn [[a b]] (pos? (- b a 1))))
       (take-while (fn [[a b]] (zero? (- b a 1))))
       count
       inc))

(defn card-values-descending [hand]
  (->> hand
       (map (comp str first))
       (map value->ordinal)
       (sort-by -)))

(defn highest-card-value [hand]
  (->> hand
       card-values-descending
       first))

(defn royal-flush [hand]
  (when (and (= [5 1] (same-suit hand))
             (has-ten? hand)
             (has-jack? hand)
             (has-queen? hand)
             (has-king? hand)
             (has-ace? hand))
    [10]))

(defn straight-flush [hand]
  (when (and (= [5] (same-suit hand))
             (= 5 (count-consecutive hand)))
    [9 (highest-card-value hand)]))

(defn value-with-frequency [freq hand]
  (dev/log-off "looking freq" freq)
  (some->> hand
           (map (comp str first))
           frequencies
           dev/probe-off
           (filter (fn [[k v]] (= v freq)))
           ffirst
           value->ordinal
           ))

(defn two-kind-with-frequency [freq hand]
  (->> hand
       (map (comp str first))
       frequencies
       (filter (fn [[k v]] (= v freq)))
       (take 2)
       (map first)
       (map value->ordinal)
       (sort-by -)
       ))

(defn four-of-a-kind [hand]
  (when (= [4 1] (same-value hand))
    (let [kind-1 (value-with-frequency 4 hand)
          kind-2 (value-with-frequency 1 hand)]
      [8 kind-1 kind-2])))

(defn full-house [hand]
  (when (= [3 2] (same-value hand))
    (let [kind-1 (value-with-frequency 3 hand)
          kind-2 (value-with-frequency 2 hand)]
      [7 kind-1 kind-2])))

(defn flush [hand]
  (when (= [5] (same-suit hand))
    (into [6] (card-values-descending hand))))

(defn straight [hand]
  (when (= 5 (count-consecutive hand))
    [5 (highest-card-value hand)]))

(defn three-of-a-kind [hand]
  (let [values (same-value hand)]
    (when (or (= [3 2] values)
              (= [3 1 1] values))
      [4 (value-with-frequency 3 hand) (or (value-with-frequency 2 hand)
                                           (drop 3 (card-values-descending hand)))])))

(defn two-pairs [hand]
  (when (= [2 2 1] (same-value hand))
    (conj (into [3] (two-kind-with-frequency 2 hand))
          (value-with-frequency 1 hand))
    ))

(defn pair [hand]
  (when (= [2 1 1 1] (same-value hand))
    (into [2 (value-with-frequency 2 hand)]
          (drop 2 (card-values-descending hand)))))

(defn high-card [hand]
  (when (= [1 1 1 1 1] (same-value hand))
    (into [1] (card-values-descending hand))))

(def resolvers [royal-flush straight-flush four-of-a-kind full-house flush straight three-of-a-kind two-pairs pair high-card])

(defn resolve-hand [hand]
  (loop [idx 0]
    (let [resolve-f (nth resolvers idx)
          res (resolve-f hand)]
      (if res
        res
        (recur (inc idx))))))

(defn winner [res-1 res-2]
  (let [first-diff (->> (interleave res-1 res-2)
                        (partition 2)
                        (map (partial apply -))
                        (drop-while zero?)
                        dev/probe-off
                        first)]
    (dev/log-off "first-diff" first-diff)
    (cond
      (pos? first-diff) 1
      (neg? first-diff) 2
      :else 3
      )))

(defn -play-game [dbg? line]
  (let [line (str/split line #" ")
        player-1-hand (take 5 line)
        player-2-hand (drop 5 line)
        _ (when dbg?
            (dev/log player-1-hand player-2-hand))
        player-1-res (resolve-hand player-1-hand)
        player-2-res (resolve-hand player-2-hand)
        _ (when dbg?
            (dev/log player-1-res player-2-res))
        res (winner player-1-res player-2-res)]
    (when (= 3 res)
      (dev/log player-1-hand player-2-hand)
      (dev/log player-1-res player-2-res))
    res))

(def play-game (partial -play-game false))
(def debug-game (partial -play-game true))
