(ns poker.dev
  (:require [clojure.pprint :as pprint]))

(def log-pr println)

(def n-able? (every-pred coll? (complement map?)))

;;
;; If it is a map (when it shouldn't be) I don't usually want the 'has elements in it'
;; test to pass. Usually a map is a single item, so it is the wrong type to be asking `seq` of.
;; Use this rather than seq
;;
(defn least-one? [x]
  (and (n-able? x) (seq x)))

;;
;; Need to loosen our checking for Fulcro Inspect, hence need this marker.
;; Change to (complement nil?) when don't have Fulcro Inspect loaded.
;; Having to nil? basically turns off the rest of the checks.
;; Solution - just don't assert, use assert-warn instead
;;
#_(def fi-lax? nil?)

;;
;; This means it is not {}!
;;
(defn filled-map? [x]
  (and (map? x) (seq x)))

(defn err-empty
  ([x]
   (assert x "Can't check if empty when nil")
   (assert (n-able? x))
   (assert (seq x) "Can't assign empty")
   x)
  ([msg x]
   (assert x "Can't check if empty when nil")
   (assert (n-able? x))
   (assert (seq x) (str "Can't assign empty, msg: " msg))
   x))

;;
;; Using apply to get devtools to format it properly
;;
(defn warn [& args]
  (assert (seq args) "Don't call warn with no args i.e. say something")
  (assert (some? (first args)) "Don't warn on nothing i.e. say something")
  (apply log-pr "WARN:" args))

(defn err [& args]
  (apply log-pr "PROBLEM:" args)
  (throw (ex-info "Correct Problem" {})))

;;
;; Will have to use this instead of asserts in all our components
;;
(defn assert-warn [pred-res & args]
  (when (not pred-res)
    (warn args)))

(defn assert-warn-off [pred-res & args])

(defn chk-v! [v]
  (assert v)
  (assert (n-able? v) v)
  (assert (seq v) v)
  (assert (every? (complement nil?) v) v))

;;
;; Fixing a terrible discovery that `(get-in {} nil)` will
;; just go on merrily, leading to bugs difficult to track down
;;
(defn get-inn
  ([st v]
   (chk-v! v)
   (clojure.core/get-in st v))
  ([st v default]
   (chk-v! v)
   (clojure.core/get-in st v default)))

;;
;; TODO
;; These two functions are only for clj, so ought to move them
;;

(def width 120)

#?(:clj  (defn pp-str
           ([n x]
            (binding [pprint/*print-right-margin* n]
              (-> x pprint/pprint with-out-str)))
           ([x]
            (pp-str width x)))
   :cljs (def pp-str identity))

#?(:clj  (defn pp
           ([n x]
            (binding [pprint/*print-right-margin* n]
              (-> x pprint/pprint)))
           ([x]
            (pp width x)))
   :cljs (def pp println))

;;
;; Hide the clutter briefly
;;
(defn pp-hide
  ([n x])
  ([x]))

(def hard-error? true)

(defn err-warn [predicate-res & msg]
  (if-not predicate-res
    (if hard-error?
      (assert false msg)
      (do
        (apply log-pr "WARNING:" msg)
        predicate-res))
    predicate-res))

(defn first-no-less [xs]
  (assert (least-one? xs) "Don't even have one: purposeful crash")
  ;; Get a crash like this, but easier to debug with your own message!
  (nth xs 0))

(defn type-and-value [value]
  (if (-> value fn? not)
    ["type" (type value) "value" value]
    []))

(defn- -one-only [f xs msgs]
  (f (= [true false] ((juxt
                        #(boolean (seq (take 1 %)))
                        #(boolean (seq (take 1 (drop 1 %))))) xs))
     (if (nil? xs)
       ["Expect to be one exactly, but got a nil" msgs]
       ["Expect to be one exactly, but got:" xs "," msgs]))
  (first xs))

(defn assert-not-macro [pred-res & args]
  (when (not pred-res)
    #?(:cljs (throw (js/Error. (str "Assert failed: " (pr-str args))))
       :clj (throw (new AssertionError (str "Assert failed: " (pr-str args)))))))

(defn one-only [xs & msgs]
  (-one-only assert-not-macro xs msgs))

(defn one-only-warn [xs & msgs]
  (-one-only assert-warn xs msgs))

(defn- -first-no-more [f xs]
  (f (n-able? xs))
  (f (= nil (second xs))
          (str "Only supposed to be one. However:\nFIRST:\n" (first xs) "\nSECOND:\n" (second xs)))
  (first xs))

(defn first-no-more [xs]
  (-first-no-more assert-not-macro xs))

(defn first-no-more-warn [xs]
  (-first-no-more assert-warn xs))

(defn summarize [x]
  (cond
    (map? x) (let [counted (count x)]
               (if (> counted 5)
                 (str counted " map-entries...")
                 (->> x
                      (map (fn [[k v]]
                             [k (summarize v)]))
                      (into {}))))
    (coll? x) (let [counted (count x)]
                (if (> counted 5)
                  (str counted " items...")
                  x))
    :else x))

(declare log)
(defn chk-dup-hof []
  (log "Going to be checking dups")
  (let [keys-atom (atom #{})]
    (fn [key]
      (let [keys @keys-atom]
        (when (keys key)
          (err "dup key" key ", already collected" (count keys) "min" (apply min keys) "max" (apply max keys)))
        (swap! keys-atom conj key)))))

;;
;; Order a map's keys randomly, to prove that need an ordered map
;;
(defn jumble-up-map [m]
  (assert (map? m))
  (->> (loop [in (mapv identity m)
              out []]
         (if (seq in)
           (let [picked (rand-nth in)]
             (recur (vec (remove #(= % picked) in)) (conj out picked)))
           out))
       (into {})))

(defmacro locals [] (into {} (map (juxt (comp keyword name) identity)) (keys &env)))

(declare probe-on)
(declare probe-off)

(defn probe-count-on [xs]
  (log-pr "COUNT" (count xs))
  xs)

(defn probe-count-off [xs]
  xs)

(defn probe-first-on [xs]
  (log-pr "FIRST" (pp-str (first xs)))
  xs)

(defn probe-first-off [xs]
  xs)

(defn probe-first-n-on [n xs]
  (println "Try take" n "from" (count xs) "\n" (pp-str (take n xs)))
  xs)

(defn probe-first-n-off [n xs]
  xs)

(defn err-nil-probe
  ([x]
   (assert x "Can't assign nil (or false)")
   x)
  ([x & msg]
   (assert x (apply str "Can't assign nil (or false), msg: " msg))
   x))

(defn err-fn-probe [f msg]
  (fn [x]
    (assert (not (f x)) (str msg ", got: <" x ">"))
    x))

;;
;; Using apply to get devtools to format it properly
;; Using just log (w/out -on) means it is permanent
;;
(defn log [& args]
  (apply log-pr args))

;;
;; Using -on means we could turn it off, or into debug stuff
;;
(def log-on log)

(defn log-off [& _])

(defn probe-f-on [f x & msgs]
  (apply log-pr (f x) "<--" msgs)
  x)

(defn probe-f-off [f xs & msgs])

(defn probe-off
  ([x]
   x)
  ([x & msgs]
   x))

(defn probe-on
  ([x]
   (-> x
       pp)
   x)
  ([x & msgs]
   (apply log-pr x "<--" msgs)
   x))
