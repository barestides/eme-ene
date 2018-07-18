(ns eme-ene.user
  (:require [clojure.repl :refer :all]
            [eme-ene.generation
             [melody :as mel-gen]
             [track :as track-gen]]
            [eme-ene.arrangement :as arrangement]
            [eme-ene.generation.melody :as mel-gen]
            [eme-ene.midi :as midi]
            [eme-ene.analyzer :as a]
            [eme-ene.player :as player]
            [eme-ene.sample.beats :as beats]
            [eme-ene.sample.melodies :as mels]
            [eme-ene.util.util :as util]
            [eme-ene.util.pitch :as pu]
            [eme-ene.util.constants :refer :all]))

(def config
  {:mode :locrian
   :tonic 60
   :mode-adherence 1.0
   ;;relative to the tonic, if tonic is c4 and floor is 6, no notes lower than f#3(?) will be played
   :floor 12
   :ceiling 12

   :smoothness-index 4.0

   :len-beats 4.0
   ;;The generator will only place notes as fine as the `beat-granularity` is specified.
   :avail-durs [:s :e]
   :pulse :q})

(def dest-config
  ;;destinations might not be necessary, but it makes more sense for using this
  {:first-pitch (pu/pitch-map-for-name :f#4)
   :transition-configs  [{:saturation 1.0
                          :direction :up
                          :target :d#
                          :inversions 0
                          :pulse :q
                          :possible-durs [:e :s]
                          :len-beats 3.0}
                         {:saturation 1.0
                          :direction :down
                          :inversions 1
                          :pulse :q
                          :possible-durs [:e :s]
                          :target :c#
                          :len-beats 2.0}]
   :key [:c :minor]})

(def controls {0 {:swing 0}
               1 {:swing 0}
               2 {:swing 0}})

(def state
  (apply player/init-state {:bpm 90 :pulse :q} controls track-gen/base-bb-tracklist))

(defn scale-midi
  [midi-vel min max]
  (+ (/ (* midi-vel (- max min)) 127) min))

(def notes->control-fns
  {20 (fn [state val]
        (swap! (:controls state) assoc-in [0 :swing] (scale-midi val -0.05 0.05)))
   21 (fn [state val]
        (swap! (:controls state) assoc-in [1 :swing] (scale-midi val -0.05 0.05)))
   22 (fn [state val]
        (swap! (:controls state) assoc-in [2 :swing] (scale-midi val -0.05 0.05)))})

(def sysex-control-fns
  {:play (fn [state] (reset! (:playing? state) false))})

(defn start-player
  []
  (midi/add-controls! state notes->control-fns)
  (midi/add-sysex-controls! state sysex-control-fns)
  (player/start-playing state))

(defn try-mel
  []
  (let [mel ;; (mel-gen/mel-gen config)
        ;; mel2 (mel-gen2 config2)
        (mel-gen/destination-mel-gen dest-config)
        piano-track (track-gen/inst-track mel :zyn-piano)
        ;; bass-track (cmmge.midi/inst-track mel2 :bass)
        ]
    (util/spy (a/smoothness-index mel :q))
    ;;this is just for messing around with. I don't think you'd ever want to play to melodies that were generated
    ;;separately, even with similar configs.
    ;;melodies that are meant to be played together should have had one melody be used to generate the other
    ;;similar to how "real" music is made
    (player/play-tracks [piano-track])))

(defn try-drum
  []
  (let [tracklist (track-gen/drum-tracklist (arrangement/elaborate-beat track-gen/base-sixteenth-map :q))]
    (player/play-tracks tracklist)))
