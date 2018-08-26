(ns eme-ene.ug-puller
  (:require [clojure.string :as s]
            [faconne.core :as f]
            [eme-ene.util.constants :refer :all]
            [eme-ene.util.util :refer :all]))

(def ug-url "https://tabs.ultimate-guitar.com/tab/air/la_femme_dargent_bass_346036")

(def ug-tab
 "g|------|------------------------------------------------------|
  d|------|-----7-9---9p7---7--9---------------------------------|
  a|------|-7h9-----------9-------9--9p7--5h7--------------------|
  E|---/7-|-----------------------------------5-5---5-5---5/7--7-|")

(defn remove-whitespace
  [string]
  (s/replace string #" " ""))

(defn is-digit? [char]
  (Character/isDigit char))

(def frets->notes
  ;;sure we could use a vector, but this is clearer
  {:E {0 :e1
       1 :f1
       2 :f#1
       3 :g1
       4 :g#1
       5 :a1
       6 :a#1
       7 :b1
       8 :c2
       9 :c#2
       10 :d2
       11 :d#2
       12 :e2
       13 :f2


       }}

  )


(defn string-and-fret-to-note
  [string fret]
  )

(defn tab->notes
  [tab]
  (let [strings (-> tab
                    remove-whitespace
                    s/split-lines)
        notes-with-keys (mapv (fn [string]
                                (let [string-key (-> string
                                                     first
                                                     str
                                                     keyword)]
                                  (mapv (fn [note] [string-key note]) string)))
                              strings)
        only-notes-on-strings
        (remove nil? (apply mapv (fn [& notes]
                                   (first (filter (fn filter-digs [[key note]]
                                                    (Character/isDigit note)) notes)))
                            notes-with-keys))]


    )
  )



(def therest
  "g|----------|
  d|----------|
  a|----------|
  E|-2--5--7--|

  g|---------------------|----|
  d|---------------------|----|
  a|---------------------|----|
  E|--7--7---2--5--7-----|-7/-|

  g|-------------------------|--------------------------------------|
  d|-------------------------|-------12--14---14b--14---14b--p12----|
  a|-----5-7b-----7b--p5-----|---14------------------------------14-|
  E|--7-------------------7--|--------------------------------------|

  g|---------------14---16--16p14----14--16------------------------------------|
  d|---------14h16----------------16---------16--16p14--12h14------------------|
  a|-14--14----------------------------------------------------12-12--12-12--12|
  E|---------------------------------------------------------------------------|")
