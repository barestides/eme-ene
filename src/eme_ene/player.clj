(ns eme-ene.player
  (:require [overtone.music.pitch :as p]
            [overtone.music.time :as t]
            [overtone.music.rhythm :as r]
            [overtone.at-at :as at-at]
            [overtone.algo.chance :as chance]
            [eme-ene.util.util :as util]
            [eme-ene.analyzer :as a]
            [eme-ene.util.chance :as cu]
            [eme-ene.midi :as midi]
            [eme-ene.util.constants :refer :all]))

(def player-state
  {:metronome (r/metronome 90)
   :pulse :q
   :playing (atom true)
   :controls (atom {})})

(def pool (at-at/mk-pool))

(defn swing
  [real-dur]

  (+ (chance/ranged-rand (+ (* 0.70 0.15 real-dur)) 0)
     (* 0.47 real-dur)))

(defn play-drum-pattern
  [state beat pattern inst-fn]
  (let [{nome :metronome pulse :pulse} state
        {:keys [dur rest?]} (first pattern)
        real-dur (float (/ (dur nice-names->note-values)
                           (pulse nice-names->note-values)))
        next-note (+ (+ beat real-dur) (swing real-dur))]
    (when-not rest? (at-at/at (nome beat) inst-fn pool))
    (when (not-empty (rest pattern))
      (t/apply-by (nome next-note) #'play-drum-pattern [state next-note (rest pattern) inst-fn]))))

;;melodic tracks are different because the inst fn needs the specific note and the duration.
;;it's not just an impulse like percussive

;;jinkies probs move this to a utils ns
;;also make it not fuck ugly
(defn real-dur
  [dur-note pulse nome]
  (let [dur-to-pulse (float (/ (dur-note nice-names->note-values)
                               (pulse nice-names->note-values)))]

    (* (float (/ dur-to-pulse (float (/ (r/metro-bpm nome)
                                        60))))
       1000)))

(defn play-one-note
  [state inst-fn {:keys [pitch dur rest?]}]
  (let [{:keys [pulse metronome]} state
        real-dur (real-dur dur pulse metronome)
        midi-note (:midi pitch)]
    (when-not rest? (inst-fn midi-note real-dur))))

(defn play-chord
  [state chord inst-fn]
  (doseq [note chord]
    (play-one-note state inst-fn note)))

(defn play-melodic-pattern
  [state beat pattern inst-fn]
  (let [note-or-chord (first pattern)
        dur (or (-> note-or-chord
                    :dur
                    nice-names->note-values)
                (apply max (map (comp nice-names->note-values :dur) note-or-chord)))
        {:keys [pulse metronome]} state
        ;;this is so ugly but idgaf
        dur-to-pulse (float (/ dur
                               (pulse nice-names->note-values)))
        next-note (+ beat dur-to-pulse)]
    (at-at/at (metronome beat)
              ;;We'll treat chords as vectors for now
              (if (map? note-or-chord)
                #(play-one-note state inst-fn note-or-chord)
                #(play-chord state note-or-chord inst-fn))
              pool)

    (when (not-empty (rest pattern))
      (t/apply-by (metronome next-note) #'play-melodic-pattern
                  [state next-note (rest pattern) inst-fn]))))

;;I bet we can do some cool macro shit to allow us to have special chars denoting a rest, rather than
;;calling a function that just returns a normal note map
;;alternatively, pitch could just be nil, and we could check on that
;;alternatively, could have velocity as part of the note map, and that could be 0
;;not a big fan of the latter though

(defn play-drum-track
  [state track]
  (let [{:keys [pulse-note metronome]} state
        {:keys [inst-fn pattern]} track]
    (play-drum-pattern state (metronome) pattern inst-fn)))

(defn play-track
  [state track]
  (let [beat ((:metronome state))
        {:keys [inst-fn pattern inst-type]} track
        player (case inst-type
                 :percussive play-drum-pattern
                 :melodic play-melodic-pattern)]
    (player state beat pattern inst-fn)))

;;`tracklist` - vector of maps; each map is a `track`
;;each `track` contains:
;;`pattern` - that indicates what notes should be played when
;;`inst-type` - which can be percussive or melodic
;;`inst-fn`, which is what actually plays the sound. this function should at least take amplitude
;;`name` friendly name optional for now
;;           in the case of melodic instruments, it should take pitch as well

;;I don't know if having the entire track in the pattern makes sense, you could indicate when a track
;;starts and stops? I don't think there's any problem with having multi bar rests though.

;;A lot of this comes down to what to express as data, and what to express
;;as transformations (functions), which I think is a fundamental concept in programming.

;;if it's data, I think it's easier to generate, makes realtime modulation difficult
;;no matter what, at some point it needs to get to being data, so we'll start with that.

(defn play-tracks
  [tracklist]
  (r/metro-start (:metronome player-state) 0)
  (util/spy tracklist)
  (doseq [track tracklist]
    (play-track player-state track)))

;;probably need to pass the state around everywhere and just pull out what we need
;;should maybe also have a config map for stuff that won't change
(def state
  {:tracks (atom {})
   ;;possibly get rid of later
   ;; :insts (atom {})
   :controls (atom {})
   :nome (r/metronome 110)
   :playing? (atom false)
   :pulse :q})

(defn play-pattern2
  [state pattern inst cur-beat track-id]
  (let [{:keys [nome pulse playing?]} state
        inst-fn (midi/get-inst-fn inst)
        {:keys [dur rest? vel]} (first pattern)
        real-dur (float (/ (dur nice-names->note-values)
                           (pulse nice-names->note-values)))
        swing (get-in @(:controls state) [track-id :swing])
        next-beat (+ cur-beat real-dur swing)]
    (if (not vel)
      (prn "hey there's no velocity on the note. For whatever reason I can't throw an exception here")
      (do (when (and @playing? (not rest?))
            (at-at/at (nome cur-beat) #(inst-fn vel) pool))
          (when (and @playing? (not-empty (rest pattern)))
            (t/apply-by (nome next-beat) #'play-pattern2 [state (rest pattern) inst next-beat track-id]))))))

(defn play-track2
  [state track-id beat]
  (let [track (get @(:tracks state) track-id)
        nome (:nome state)
        {:keys [pattern inst]} track
        pattern-length (a/melody-length (:pattern track) (:pulse state))
        next-beat (+ pattern-length beat)]
    (at-at/at (nome beat) #(play-pattern2 state pattern inst beat track-id) pool)
    (when @(:playing? state)
      (t/apply-by (nome next-beat) #'play-track2 [state track-id next-beat]))))

;;the two things different here are that we loop once the pattern is finished, and that the pattern can change
;;we are going to start by making it so changes to the pattern are not realized until the next time the pattern is
;;played.
;;So we deref the pattern, play it, then look at what the pattern is again, and play that.
;;we can still reference controls like for swing b/c that is computed when playing
(defn play-tracks2
  [state]
  (doseq [track-id (keys @(:tracks state))]
    (play-track2 state track-id ((:nome state)))))

(defn start-playing
  [state]
  (if (not-empty @(:tracks state))
    (do
      (reset! (:playing? state) true)
      (r/metro-start (:nome state) 0)
      (play-tracks2 state))
    (throw (Exception. "No tracks to play."))))

(defn pause!
  [state]
  (reset! (:playing? state) false))

(defn add-track!
  [state track]
  (let [tracks (:tracks state)
        id (if (empty? @tracks)
             0
             (inc (apply max (keys @tracks))))]
    (swap! tracks assoc id track)))

(defn init-state
  [config controls & tracks]
  (let [{:keys [bpm pulse]} config
        state {:tracks (atom {})
               :controls (atom controls)
               :nome (r/metronome bpm)
               :playing? (atom false)
               :pulse pulse}]
    (doseq [track tracks]
      (add-track! state track))
    state))

(defn remove-track!
  [state track-id]
  (swap! (:tracks state) dissoc track-id))
