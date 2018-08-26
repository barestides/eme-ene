(ns eme-ene.util.rhythm
  (:require [clojure.math.combinatorics :as combos]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :as util]))

(defn domain-for-dur-in-len
  [dur len-beats]
  (->> dur
       (/ len-beats)
       Math/floor
       inc
       range
       (into [])))

;;this could definitely be more efficient. We're just getting all possible valid integer values for each duration
;;and seeing what combinations equal the len-beats. P terrible..
(defn allowable-dur-counts
  [possible-durs len-beats pulse]
  (let [names->durs (select-keys (nice-names->note-fracs pulse) possible-durs)
        coeffs (mapv #(domain-for-dur-in-len (second %) len-beats) names->durs)
        all-combos (apply combos/cartesian-product coeffs)]
    (filter #(= len-beats (float (apply + (map * % (map second names->durs))))) all-combos)))
