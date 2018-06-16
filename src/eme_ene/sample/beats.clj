(ns eme-ene.sample.beats
  (:require [eme-ene.generation.track :as track]))

;;Syncopation research examples
;; https://github.com/barestides/eme-ene/issues/14
(def right-on
  [{:dur :q} {:dur :q} {:dur :q} {:dur :q}])

(def rest-1
  [{:dur :w :rest? true}])

(defn syncopation-experiment
  [pattern]
  (track/drum-tracklist
   {:hh (track/times right-on 2)
    :k (into rest-1 pattern)}))

(def unsyncopated
  (syncopation-experiment right-on))

(def ands
  (syncopation-experiment (track/times [{:dur :e :rest? true} {:dur :e}] 4)))

(def ees
  (syncopation-experiment (track/times [{:dur :s :rest? true} {:dur :s} {:dur :e :rest? true}] 4)))

(def uhhs
  (syncopation-experiment (track/times [{:dur :de :rest? true} {:dur :s}] 4)))

(def off-beat-hats
  (track/drum-tracklist
   {:hh (track/times [{:dur :e :rest? true} {:dur :e :rest? true}] 4)
    :k (track/times [{:dur :q} {:dur :q :rest? true}] 2)
    :s (track/times [{:dur :q :rest? true} {:dur :q}] 2)}))

(def base-sixteenth-map {:hh (track/times [{:dur :s}] 16)
                         :k [{:dur :q} {:dur :e :rest? true} {:dur :s :rest? true} {:dur :s} {:dur :q} {:dur :q :rest? true}]
                         :s (track/times [{:dur :q :rest? true} {:dur :q}] 2)})
(def base-sixteenth-bb
  (track/drum-tracklist base-sixteenth-map))
