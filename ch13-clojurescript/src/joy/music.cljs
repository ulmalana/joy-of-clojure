(ns joy.music)

(defn soft-attack
  "Return a gain node that goes from silent at time <delay> up to
  <volume> in 50 ms, then ramps back down to silent after <duration>"
  [ctx {:keys [volume delay duration]}]
  (let [node (.createGain ctx)]
    (doto (.-gain node)
      (.linearRampToValueAtTime 0 delay)
      (.linearRampToValueAtTime volume (+ delay 0.05))
      (.linearRampToValueAtTime 0 (+ delay duration)))
    node))

(defn sine-tone
  "Return an oscillator that plays starting at <delay> for <duration> s"
  [ctx {:keys [cent delay duration]}]
  (let [node (.createOscillator ctx)]
    (set! (-> node .-frequency .-value) 440)
    (set! (-> node .-detune .-value) (- cent 900))
    (.start node delay)
    (.stop node (+ delay duration))
    node))

(defn connect-to
  "Connect the outpu of node1 to the input of node2, returning node2"
  [node1 node2]
  (.connect node1 node2)
  node2)

(defn woo
  "Play a 'woo' sound, like a glass harp"
  [ctx note]
  (let [linger 1.5
        note (update-in note [:duration] * linger)]
    (-> (sine-tone ctx note)
        (connect-to (soft-attack ctx note)))))

(def make-once (memoize (fn [ctor] (new ctor))))

(defn play!
  "Kick off playing  a seq of notes. note-fn must take 2 args,
  an AudioContext object and a map representing one note to play.
  It must return an AudioNode object that will play that note."
  [note-fn notes]
  (if-let [ctor (or (.-AudioContext js/window)
                    (.-webkitAudioContext js/window))]
    (let [ctx (make-once ctor)
          compressor (.createDynamicsCompressor ctx)]
      (let [now (.-currentTime ctx)]
        (doseq [note notes]
          (->
           (note-fn ctx (update-in note [:delay] + now))
           (connect-to compressor))))
      (connect-to compressor (.-destination ctx)))
    (js/alert "Sorry this browser doesnt support AudioContext")))

(play! woo [{:cent 1100, :duration 1, :delay 0, :volume 0.4}])

(defn pair-to-note
  "Return a note map for the given tone and duration"
  [[tone duration]]
  {:cent (* 100 tone)
   :duration duration
   :volume 0.4})

(defn consecutive-notes
  "Take a seq of note maps that have no :delay and return them with
  correct :delay so that they will play in the order given"
  [notes]
  (reductions (fn [{:keys [delay duration]} note]
                (assoc note :delay (+ delay duration)))
              notes))

(defn notes
  [tone-pairs]
  "Return a seq of note maps at moderate tempo for the given seq of tone-pairs"
  (let [bpm 360
        bps (/ bpm 60)]
    (->> tone-pairs
         (map pair-to-note)
         consecutive-notes
         (map #(update-in % [:delay] / bps))
         (map #(update-in % [:duration] / bps)))))

(defn magical-theme
  "A seq of notes for a magical theme"
  []
  (notes
   (concat
    [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
    [[21 6] [18 6] [16 3] [19 1] [18 2] [14 4] [17 2] [11 10]]
    [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
    [[26 4] [25 2] [24 4] [20 2] [24 3] [23 1] [22 2] [10 4]
     [19 2] [16 10]])))

(defn ^:export go []
  (play! woo (magical-theme)))
