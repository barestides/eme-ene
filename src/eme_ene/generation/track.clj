(ns eme-ene.generation.track
  (:require [faconne.core :as f]
            [eme-ene.midi :as midi]
            [eme-ene.util.constants :refer :all]))

;;this isn't really generation, more like helpers for making tracks.
;;There will be generation, maybe the stuff here should move to a new ns

(def inst-fns
  {:k #(midi/drumkv1-inst :kick)
   :s #(midi/drumkv1-inst :snare)
   :hh #(midi/drumkv1-inst :hi-hat)
   :bass (partial midi/qsynth-inst :bass)
   :piano (partial midi/qsynth-inst :piano)
   :zyn-piano (partial midi/zynaddsubfx-inst :piano)
   :zyn-bass (partial midi/zynaddsubfx-inst :bass)})

(defn times
  "Loop a pattern some number of times"
  [pattern num-times]
  (into [] (apply concat (repeat num-times pattern))))

(defn drum-tracklist
  [patterns]
  (f/transform patterns
               {inst pattern}
               [{:inst-type :percussive
                 :inst-fn (inst inst-fns)
                 :pattern pattern}]))

(def kick-track {:name "Kick"
                 :inst-type :percussive
                 :inst-fn (:k inst-fns)
                 :pattern (times [{:dur :q} {:dur :q :rest? true} {:dur :q} {:dur :q :rest? true}]
                                 2)})

(def snare-track {:name "Snare"
                  :inst-type :percussive
                  :inst-fn (:s inst-fns)
                  :pattern (times [{:dur :q :rest? true} {:dur :q} {:dur :q :rest? true} {:dur :q}]
                                  2)})

(def bass-track {:name "Bass"
                 :inst-type :melodic
                 :inst-fn (:bass inst-fns)
                 :pattern '({:pitch :eb2 :dur :e}
                            {:pitch :f2 :dur :e}
                            {:pitch :ab2 :dur :e}
                            {:pitch :bb2 :dur :e}
                            {:pitch :f2 :dur :e}
                            {:dur :e :rest? true}
                            {:dur :de :rest? true}
                            {:pitch :f2 :dur :s}

                            {:pitch :eb2 :dur :e}
                            {:pitch :f2 :dur :e}
                            {:pitch :ab2 :dur :e}
                            {:pitch :bb2 :dur :e}
                            {:pitch :f2 :dur :e}
                            {:dur :e :rest? true}
                            {:dur :de :rest? true})})

(def sample-tracklist [kick-track
                       snare-track
                       bass-track])

(defn inst-track
  [mel inst]
  {:inst-type :melodic
   :inst-fn (inst inst-fns)
   :pattern mel})

(defn rest-bars
  [num-bars]
  ;;assume 4/4
  (repeat num-bars {:dur :w :rest? true}))

(defn simple-build
  "Increment the number of playing tracks"
  [tracks]
  ;;assuming all the tracks' patterns are the same length in beats
  (map-indexed #(update %2 :pattern into (rest-bars (* % 1))) tracks))

;new track ex, for live player
(def hh-track
{:inst {:app :drumkv1
        :drum :hi-hat
        :type :percussive}
 :pattern [{:dur :s :vel 50}
           {:dur :s, :rest? true}
           {:dur :s :vel 50}
           {:dur :s, :rest? true}
           {:dur :s :vel 75}
           {:dur :s :vel 100}
           {:dur :s :vel 127}
           {:dur :s, :rest? true}
           {:dur :s :vel 120}
           {:dur :s, :rest? true}
           {:dur :s :vel 30}
           {:dur :s, :rest? true}
           {:dur :s :vel 30}
           {:dur :s, :rest? true}
           {:dur :s :vel 30}
           {:dur :s, :rest? true}]})

(defn drum-tracklist2
  [drum->pattern]
  (f/transform drum->pattern
               {inst pattern}
               [{:inst {:app :drumkv1
                        :drum inst
                        :type :percussive}
                 :pattern pattern}]))

(defn velocitize
  [pattern]
  (reduce (fn [so-far note]
            (let [vel (if (= (mod (count (remove :rest? so-far)) 2) 1)
                        70
                        110)]
              (conj so-far (assoc note :vel vel))))
          [] pattern))

;;these drum->pattern maps are useful for mocking stuff up, but need to be added to in order to be played
(def base-sixteenth-map
  {:hi-hat (velocitize (times [{:dur :s}] 16))
   :kick (velocitize [{:dur :q} {:dur :e :rest? true} {:dur :s :rest? true} {:dur :s} {:dur :q} {:dur :q :rest? true}])
   :snare (velocitize (times [{:dur :q :rest? true} {:dur :q}] 2))})

(def base-bb-tracklist
  (drum-tracklist2 base-sixteenth-map))
