(ns eme-ene.arrangement
  (:require [eme-ene.generation.melody :as mel-gen]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :refer :all]
            [overtone.algo.chance :as chance]))

(def config {:mode :ionian
             ;; :mode-adherence 1.0
             :tonic 60
             :floor 12
             :ceiling 12
             :smoothness-index 6.0
             :len-beats 4.0
             :beat-granularity :s
             :pulse :q})

;;changes:
;; same rhythmic pattern, different pitches entirely
;; syncopate
;; use directions of pitch movement, but different intervals


(defn elaborate-beat
  [drum-map pulse]
  (let [{:keys [hh k s]} drum-map
        new-hh (reduce (fn [pattern note]
                         (let [beat (/ (apply + (map (comp nice-names->note-values :dur) pattern))
                                       (nice-names->note-values pulse))
                               ;;based on the strength of the beat, play the note or do a rest
                               ;;We could use this for generation, we're just iterating through every possible place
                               ;;a note could be given granularity
                               rest? (chance/weighted-coin (case (mod beat 1)
                                                             0.0 0
                                                             0.25 1
                                                             0.5 0
                                                             0.75 1

                                                             ))]
                           (spy beat)
                           (conj pattern (if rest? {:dur (:dur note) :rest? true} note))))
                       []
                       hh)]
    (assoc drum-map :hh new-hh)))
