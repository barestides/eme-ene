(ns eme-ene.midi
  (:require [overtone.studio.midi :as omidi]
            [overtone.music.pitch :as pitch]
            [overtone.libs.event :as event]
            [overtone.at-at :as at-at]
            [overtone.music.time :as t]
            [overtone.music.rhythm :as r]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :as util]))

(def receivers (omidi/midi-connected-receivers))

;;assuming `Virtual-Raw-MIDI-4.0` is hooked up to qsynth
(def qsynth (first (filter #(re-find #":0,0" (:name %)) receivers)))
(def drumkv1 (first (filter #(re-find #":0,2" (:name %)) receivers)))
(def zynaddsubfx (first (filter #(re-find #":0,3" (:name %)) receivers)))

(def apps
  {:drumkv1 drumkv1
   :qsynth qsynth
   :zynaddsubx zynaddsubfx})

;;we should definitely track these in state and allow them to be changeable
;; https://github.com/barestides/eme-ene/issues/10
(def midi->drum {:kick 36
                 :snare 40
                 :hi-hat 42})

(def inst->qsynth-channel
  {:bass 2
   :piano 0})

(def inst->zyn-channel
  {:bass 0
   :piano 1})

;;these should be more generic, we really have just melodic and percussive, but we could have different receivers
;;remove these eventually
(defn drumkv1-inst
  [inst]
  (omidi/midi-note drumkv1 (inst midi->drum) 100 10))

(defn qsynth-inst
  [inst midi-note dur]
  (omidi/midi-note qsynth midi-note 100 dur (inst inst->qsynth-channel)))

(defn zynaddsubfx-inst
  [inst midi-note dur]
  (omidi/midi-note zynaddsubfx midi-note 100 dur (inst inst->zyn-channel)))

(defmulti get-inst-fn (fn [inst] (:type inst)))

(defmethod get-inst-fn :percussive
  [inst]
  (let [{:keys [app drum]} inst]
    (fn [vel]
      (omidi/midi-note (app apps) (drum midi->drum) vel 10))))

(defmethod get-inst-fn :melodic
  [inst]
  (let [{:keys [app channel]} inst]
    (fn [dur midi vel]
      (omidi/midi-note (app apps) midi vel dur channel))))

;; (defn get-inst-fn
;;   [app channel]
;;   (fn [{:keys [dur midi vel] :as note}]
;;     (omidi/midi-note (app apps) midi vel (or dur 10) channel)))

(def loop-state
  {:metronome (r/metronome 120)
   :pulse-note :q
   :pulse (atom 1000)
   :playing? (atom true)})

(defn scale-pulse
  [current-pulse vel]
  (+ (* -7.48 vel) 1149.6))

(def step-skip-pct (atom 0.5))
(def up-down-pct (atom 0.5))

(event/on-event [:midi-device
                 "ALSA (http://www.alsa-project.org)" "VirMIDI [hw:0,2,7]" "VirMIDI, VirMidi, Virtual Raw MIDI" 0]
                (fn [e]
                  (util/spy (get-in e [:sysex :data]))
                  ;; (when (= (:note e) 20)
                  ;;   (let [new-val (* (:velocity e) (float (/ 1 127)))]
                  ;;     (reset! step-skip-pct new-val)))
                  )
                ::step-skip-pct-controller)

(event/on-event [:midi-device "ALSA (http://www.alsa-project.org)" "VirMIDI [hw:0,2,7]"
                 "VirMIDI, VirMidi, Virtual Raw MIDI"

                 ]
                (fn [e]
                  (prn "we goin")
                  ;; (when (= (:note e) 20)
                  ;;   (let [new-val (* (:velocity e) (float (/ 1 127)))]
                  ;;     (reset! step-skip-pct new-val)))
                  )
                ::step-skip-pct-controller)

(event/on-event [:midi-device "ALSA (http://www.alsa-project.org)" "VirMIDI [hw:0,2,7]"
                 "VirMIDI, VirMidi, Virtual Raw MIDI" 0 :control-change]
                (fn [e]
                  (when (= (:note e) 23)
                    (let [new-val (* (:velocity e) (float (/ 1 127)))]
                      (reset! up-down-pct new-val))))
                ::up-down-pct-controller)

(defn add-controls!
  [state notes->control-fns]
  (event/on-event
   [:midi-device "ALSA (http://www.alsa-project.org)"
    "VirMIDI [hw:0,2,14]" "VirMIDI, VirMidi, Virtual Raw MIDI" 0 :control-change]
   (fn [e]
     (when-let [control-fn (get notes->control-fns (:note e))]
             (control-fn state (:velocity e))))
   ::controller-handler)

  )

;;these are the contents of the :data key of the events when each button is pressed
(def akai-sysex-buttons
  {[127, 127, 6, 2, -9] :play
   [127, 127, 6, 6, -9] :record
   [127, 127, 6, 1, -9] :stop
   [127, 127, 6, 5, -9] :skip-back
   [127, 127, 6, 4, -9] :skip-forward})

(defn add-sysex-controls!
  "`sysex-controls` is a map from buttons (the same buttons that are the vals in `akai-sysex-buttons`)
  to functions that should be run when the specified button is pressed."
  [state sysex-fns]
  (event/on-event [:midi-device "ALSA (http://www.alsa-project.org)" "VirMIDI [hw:0,2,12]"
                   "VirMIDI, VirMidi, Virtual Raw MIDI" 0]
                  (fn [e]
                    ;;sometimes pressing the button will send two events, one that we want, and one that is nil
                    (when-let [data (get-in e [:sysex :data])]
                     (let [ ;;even though it looks like a vector, it is not
                            data (into [] data)
                            button (get akai-sysex-buttons data)]
                       (when-let [sysex-fn (button sysex-fns)]
                         (sysex-fn)))))
                  ::sysex-handler))

;;play a chord with just one keypress!
;; (event/on-event [:midi-device "ALSA (http://www.alsa-project.org)" "VirMIDI [hw:3,2,7]"
;;                  "VirMIDI, VirMidi, Virtual Raw MIDI" 0 :note-on]
;;                 (fn [e]
;;                   (omidi/midi-note qsynth (:note e) (:velocity e) 100)
;;                   (at-at/at (+ (at-at/now) 50)
;;                             #(omidi/midi-note qsynth (+ 4 (:note e)) (:velocity e) 120) pool)
;;                   (at-at/at (+ (at-at/now) 100)
;;                             #(omidi/midi-note qsynth (+ 7 (:note e)) (:velocity e) 140) pool)
;;                   (at-at/at (+ (at-at/now) 150)
;;                             #(omidi/midi-note qsynth (+ 11 (:note e)) (:velocity e) 150) pool)
;;                   (at-at/at (+ (at-at/now) 200)
;;                             #(omidi/midi-note qsynth (+ 14 (:note e)) (:velocity e) 150) pool)
;;                   )
;;                 ::keyboard-handler)
