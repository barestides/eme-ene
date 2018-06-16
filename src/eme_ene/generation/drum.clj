(ns eme-ene.generation.drum
  (:require [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :as util]))

;;"complexity levels", at 4 (quarter notes only), there four possible "slots", half are weak, half are
;;strong
;;at 8, there are 8 possible slots, again half are weak and half are strong

;;ordering from "strongest" to "weakest" would be 1, 3, 2, 4 for 4
;;for 8 would be 1,3,2,4,1and,3and,2and,4and, or 1,5,3,7,2,4,6,8

;;we want to be able to have a pattern we draw from, and every couple bars, we use that pattern, along with some
;;configuration to play a new pattern. The configuration I have in mind is saturation increase / decrease per
;;instrument, and then eventualy something with syncopation. The syncopation is more complicated, we should
;;start with just saturation

(def config
  {:syncopation-index 0.0
   :granularity :s
   :pulse :q
   :len 8.0})

(defn gen-drum-pattern
  [config]
  (let [{:keys [len pulse granularity]} config
        slots (range (* len (/ (pulse nice-names->note-values) (granularity nice-names->note-values))))]
    ;; (for [slot slots]

    ;;   )
    )

  )
