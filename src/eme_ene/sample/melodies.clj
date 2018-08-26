(ns eme-ene.sample.melodies)

(def mel-w-chords [[{:pitch {:midi 60, :name :c4, :freq 261.63}, :dur :q}
                    {:pitch {:midi 63, :name :f3, :freq 174.61}, :dur :q}
                    {:pitch {:midi 67, :name :g3, :freq 196.0}, :dur :q}]
                   {:pitch {:midi 67, :name :g3, :freq 196.0}, :dur :q}
                   [{:pitch {:midi 62, :name :c4, :freq 261.63}, :dur :q}
                    {:pitch {:midi 65, :name :f3, :freq 174.61}, :dur :q}
                    {:pitch {:midi 69, :name :g3, :freq 196.0}, :dur :q}]])

(def a1
  [{:pitch :c1 :dur :dq}
   {:pitch :d#1 :dur :e}
   {:pitch :c#1 :dur :dq}
   {:rest? true :dur :e}
   {:pitch :c1 :dur :dq}
   {:pitch :d#1 :dur :e}
   {:pitch :c#1 :dur :q}
   {:pitch :f1 :dur :q}])

;; https://drive.google.com/file/d/1bXX-mboJmjg3wxByd-OSL83BZAXZHF7i/view
(def black-milk-bassline
  [{:pitch :f# :dur :q}
   {:pitch :a :dur :e}
   {:pitch :c# :dur :e}
   {:pitch :d# :dur :q}
   {:pitch :c# :dur :q}

   {:pitch :c# :dur :e}
   {:pitch :f# :dur :e}
   {:pitch :a :dur :e}
   {:pitch :c# :dur :e}
   {:pitch :e :dur :q}
   {:pitch :d# :dur :q}

   ]
  )
