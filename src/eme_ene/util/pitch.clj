(ns eme-ene.util.pitch
  (:require [overtone.music.pitch :as pitch]
            [overtone.algo.lists :as lists]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :as util]))

(def circle-of-fifths
  [:c :g :d :a :e :b :f# :db :ab :eb :bb :f])

(defn modulate-key
  [key amount]
  (let [keys-from-key (lists/rotate (.indexOf circle-of-fifths key) circle-of-fifths)]
    ;;meh... I'm not sure how to make it better
    (if (neg? amount)
      (get (apply vector (reverse keys-from-key)) (dec (Math/abs amount)))
      (get keys-from-key amount))))

(defn midi-from-pitch-class-and-octave
  [pitch-class octave]
  (pitch/note (str (name pitch-class) octave)))

(defn convert-melody-to-new-key
  [melody-pitches from-key amount]
  (let [octave (:octave (pitch/note-info (first melody-pitches)))
        to-key (modulate-key from-key amount)
        from-tonic-midi (midi-from-pitch-class-and-octave from-key octave)
        to-tonic-midi (midi-from-pitch-class-and-octave to-key octave)
        difference (- to-tonic-midi from-tonic-midi)]
    (map #(- (pitch/note %) difference) melody-pitches)))

(defn- inc-first
  "Remove the first element, increment it by n, and append to seq."
  [elems n]
  (concat (next elems) [(+ n (first elems))]))

(defn- dec-last
  "Remove the last element, decrement it by n, and prepend to seq."
  [elems n]
  (concat [(- (last elems) n)] (butlast elems)))

;;This is a working clone of `overtone.music.pitch/invert-chord`
;;that function does not properly invert chords "negatively"
(defn invert-chord
  "Move a chord voicing up or down.

    ;first inversion
    (invert-chord [60 64 67] 1) ;=> (64 67 72)

    ; second inversion
    (invert-chord [60 64 67] 2) ;=> (67 72 76)
  "
  [notes shift]
  (cond
    (pos? shift) (recur (inc-first notes 12) (dec shift))
    (neg? shift) (recur (dec-last notes 12) (inc shift))
    (zero? shift) notes))

(defn pitch-map-for-midi
  [midi]
  (get (util/index-by :midi pitches) midi))

(defn pitch-map-for-name
  [pitch-name]
  (get (util/index-by :name pitches) pitch-name))

(defn plus-pitch
  [args]
  (let [midi-note (reduce #(+ (or (:midi %) %) (or (:midi %2) %2)) 0 args)]
    (pitch-map-for-midi midi-note)))

(defn stringify-pitch
  [pitch]
  (if pitch
    (clojure.string/lower-case (name pitch))
    ""))

(defn pitch-maps-for-pitch-class
  [pitch-class]
  (filter #(= (stringify-pitch pitch-class)
              (->> %
                   :name
                   stringify-pitch
                   (re-find #"[^\d]*")))
          pitches))

(defn pclass-for-pmap
  [pmap]
  (->> pmap
       :name
       stringify-pitch
       (re-find #"[^\d]*")
       keyword))

(defn pitch-map-for-pclass-and-octave
  [pclass octave]
  (or (first (filter #(= (stringify-pitch (:name %)) (stringify-pitch (str (name pclass) octave))) pitches))
      (throw (Exception. (str "Pitch does not exist for pitch class=" pclass " and octave=" octave)))))

(defn pitch-type
  [pitch]
  (cond (keyword? pitch)
        :name

        (integer? pitch)
        :midi

        (float? pitch)
        :freq))

(defn flesh-out-pitch
  [pitch]
  (case (pitch-type pitch)

    :name
    (pitch-map-for-name pitch)

    :midi
    (pitch-map-for-midi pitch)))

(defmacro defpitchfn
  "pitch fns always expect pitch maps (see constants/pitches), but when using them, we don't want to have to construct
  the whole map just to use the fn (like in mel-gens), that would be messy.

  this macro will transform arguments that can be midi notes (e.g. 96, 49), or note names (e.g. :f#3 :g2), into
  fully fleshed out note maps before passing them to the function.

  these arguments should contain `pitch-map` in their name.

  Forces the function to return a pitch of the same type as the first pitch-map arg. If note names are passed,
  a note name will be returned. If the fn returns a vector of pitch-maps instead of just one pitch-map, it
  will convert each pitch-map to the type input to the fn.

  I don't know if this is the best approach, but it's better than manually making the maps everywhere "
  [fn-name args body]
  `(defn ~fn-name
     ~args
     ;;I'd rather return-type be in the let, but that gets trick because it gets namespaced or something
     (def return-type# (pitch-type ~(first (filter #(re-find #"pitch-map" (str %)) args))))
     (let ~(into []
                 (apply concat (mapv (fn [arg]
                                       [arg (if (re-find #"pitch-map" (str arg))
                                              `(flesh-out-pitch ~arg)
                                              arg)])
                                     args)))
       (def result# ~body)
       (if (or (seq? result#) (vector? result#))
         (mapv return-type# result#)
         (get result# return-type#)))))

;;this could probably be more efficient
;;should also allow one to specify octaves
;; (defpitchfn closest-pitch
;;   [source-pitch-map target-pitch-class direction]
;;   (first
;;    (sort-by
;;     #(Math/abs (- (:midi source-pitch-map) (:midi %)))
;;     (filter #(case direction
;;                :up   (< (:midi source-pitch-map) (:midi %))
;;                :down (> (:midi source-pitch-map) (:midi %)))
;;             (pitch-maps-for-pitch-class target-pitch-class)))))

;; (defn pitches-between
;;   [lower upper]
;;   (filter #(< (:midi lower) (:midi %) (:midi upper)) pitches))

;; (defpitchfn pitches-between
;;   [pitch-map-limit1 pitch-map-limit2]
;;   (let [[lower upper] (sort-by :midi [pitch-map-limit1 pitch-map-limit2])]
;;     (filter #(< (:midi lower) (:midi %) (:midi upper)) pitches)))

;; (defpitchfn plus
;;   [pitch-map semitones]
;;   (pitch-map-for-midi (+ (:midi pitch-map) semitones)))


(defn closest-pitch
  [source target-class direction]
  (first
   (sort-by
    #(Math/abs (- (:midi source) (:midi %)))
    (filter #(case direction
               :up   (< (:midi source) (:midi %))
               :down (> (:midi source) (:midi %)))
            (pitch-maps-for-pitch-class target-class)))))


(defn pitches-between
  [lower upper]
  (filter #(< (:midi lower) (:midi %) (:midi upper)) pitches))

(defn pitches-between
  [pitch-map-limit1 pitch-map-limit2]
  (let [[lower upper] (sort-by :midi [pitch-map-limit1 pitch-map-limit2])]
    (filter #(< (:midi lower) (:midi %) (:midi upper)) pitches)))

(defn plus
  [pitch-map semitones]
  (pitch-map-for-midi (+ (:midi pitch-map) semitones)))

(defn sort-pitches
  [pitches]
  (sort-by :midi pitches))

;;this should be smarter once we get into modes
(def keys-pitches
  {[:c :major] #{:c :d :e :f :g :a :b}
   [:c :minor] #{:c :d :d# :f :g :g# :a#}
   }
  )

(defn in-key?
  [key pitch]
  (contains? (get keys-pitches key) (pclass-for-pmap pitch)))

(defn below-pitch?
  [pitch1 pitch2]
  (> (:midi pitch1) (:midi pitch2))
  )

(defn above-pitch?
  [pitch1 pitch2]
  (< (:midi pitch1) (:midi pitch2)))
