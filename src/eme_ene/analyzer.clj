(ns eme-ene.analyzer
  (:require [faconne.core :as f]
            [eme-ene.util.util :as util]
            [eme-ene.util.constants :refer :all]))

;;What do we want to look at?

;;variability - total note length traveled / cantus length
;;climax placement

(defn distance-traveled
  ([mel]
   (distance-traveled (mapv (comp :midi :pitch) mel) 0))
  ([[first-note second-note :as mel] total]
   (if second-note
     (recur (rest mel) (+ total (Math/abs (- first-note second-note))))
     total)))

(defn melody-length
  [mel pulse]
  (/ (apply + (map #(nice-names->note-values (:dur %)) mel)) (nice-names->note-values pulse)))

(defn smoothness-index
  [mel pulse]
  (float (/ (distance-traveled mel) (melody-length mel pulse))))

(defn count-durs
  [pattern]
  (-> pattern
      set
      count))

(defn dur-chains
  "Returns a map from each distinct duration in a pattern to the lengths of its chains.
  Ex:
  [:q :q :e :e :q]
  returns
  {:q [2 1]
   :e [2]}
  It's not pretty, but it works :shrug: "
  [pattern]
  (loop [pattern pattern
         result (f/transform (set pattern) [dur] {dur []})
         count-cur-chain 0
         cur-chain-dur (first pattern)]
    (let [dur (first pattern)]
      (if (= cur-chain-dur dur)
        (if (empty? (rest pattern))
          (update result dur conj (inc count-cur-chain))
          (recur (rest pattern) result (inc count-cur-chain) cur-chain-dur))
        (let [new-result (update result cur-chain-dur conj count-cur-chain)]
          (if (empty? (rest pattern))
            (update new-result dur conj 1)
            (recur (rest pattern) new-result 1 dur)))))))
