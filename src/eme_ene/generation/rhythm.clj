(ns eme-ene.generation.rhythm
  (:require [faconne.core :as f]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :as util]))

(def config
  {:saturation 1
   :len-beats 4
   :granularity :s
   }
  )

;;I want to be able to ascribe "syncopation" values to each available slot.
;;two things that need to be solved are:
;;what are the values? how much more syncopated are some slots than other slots.
;;some work on this has been done with the syncopation experiments in `sample.beats`
;;upon first impressions, it seems to be a logarithmic relationship between number of beat divisions and
;;"syncopatedness" of the division (UHH is more syncopated than EE, but less so than EE is to AND)
;;the other problem is more general,

;;Even if we can compute an attribute to anaylyze a collection (in this case, collections are vectors of notes),
;;how do reliably generate collections that can meet that attribute
;; we ran into it with the smoothness index in melody generator

;;we can have a map from slots->syncopated values, and pull things out of it until we reach our desired syncopated
;;level. But now how do we do saturation? I think the real problem is that saturation and syncation are more related
;;than we had initially approached this. If we play an instrument every 16th note in a measure, it won't sound
;;syncopated at all.

(def slots->sync-values
  (let [ordered-slots [0.0 1.0 2.0 3.0
                       0.5 1.5 2.5 3.5
                       0.25 1.25 2.25 3.25
                       0.75 1.75 2.75 3.75]
        vals (map #(Math/log (float %)) (range 1 17))]
    (apply merge (map (fn [k v] {k v}) ordered-slots vals))))


(def ordered-slots
  [0.0
   1.0
   2.0
   3.0
   0.5
   1.5
   2.5
   3.5
   0.25
   1.25
   2.25
   3.25
   0.75
   1.75
   2.75
   3.75])

(defn gen-notes
  [config]
  )
