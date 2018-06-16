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
            [eme-ene.util.constants :refer :all]))

(def config
  {:mode :aeolian
   :tonic 60
   :mode-adherence 1.0
   ;;relative to the tonic, if tonic is c4 and floor is 6, no notes lower than f#3(?) will be played
   :floor 12
   :ceiling 12

   :smoothness-index 4.0

   :len-beats 4.0
   ;;The generator will only place notes as fine as the `beat-granularity` is specified.
   :beat-granularity :s
   :pulse :q})

(def controls {:swing 0})

(def state
  (apply player/init-state {:bpm 120 :pulse :q} controls track-gen/base-bb-tracklist))

(def notes->control-fns
  {20 (fn [state val]
        (swap! (:controls state) assoc :swing (util/spy (/ (* val 0.25) 127))))})

(def sysex-control-fns
  {:play #(prn "you pressed play")}

  )

(defn start-player
  []
  (midi/add-controls! state notes->control-fns)
  (midi/add-sysex-controls! state sysex-control-fns)
  (player/start-playing state))

(defn try-mel
  []
  (let [mel (mel-gen/mel-gen config)
        ;; mel2 (mel-gen2 config2)
        piano-track (track-gen/inst-track mel :zyn-piano)
        ;; bass-track (cmmge.midi/inst-track mel2 :bass)
        ]
    ;;this is just for messing around with. I don't think you'd ever want to play to melodies that were generated
    ;;separately, even with similar configs.
    ;;melodies that are meant to be played together should have had one melody be used to generate the other
    ;;similar to how "real" music is made
    (player/play-tracks [piano-track])))
