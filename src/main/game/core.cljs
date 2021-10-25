(ns game.core
  (:require [game.audio :as a]
            [game.engine :as e]
            [game.waves :as w]))


(defrecord Starfield [stars w h]
  e/INeedsUpdate
  (update [_ delta-ms _ _]
    (swap! stars
           #(mapv (fn [{:keys [x z] :as star}]
                    (let [new-x (- x (* z 0.1 delta-ms))]
                      (if (< new-x 0)
                        ;; this star has moved off the screen, so re-spawn
                        (assoc star
                               :x (+ w 1)
                               :y (rand-int h)
                               :z (rand))
                        (assoc star :x new-x))))
                  %)))
  e/IProvidesRects
  (get-rects [_]
    (mapv (fn [{:keys [z] :as s}]
            (let [size (+ 1.0 (js/Math.floor (* z 4)))]
              (assoc s
                     :w size
                     :h size
                     :col (cond
                            (< z 0.3) "#333"
                            (< z 0.6) "#999"
                            :else "#bbb"))))
          @stars)))

(defn make-starfield [star-count game-width game-height]
  (let [stars (mapv (fn [_]
                      {:x (rand-int game-width)
                       :y (rand-int game-height)
                       :z (rand)})
                    (range star-count))]
    (Starfield. (atom stars)
                game-width game-height)))

(defn- dampen-to-zero [v delta-ms]
  (if (< v 0.001)
    0
    (- v (* 0.01 delta-ms))))

(defn- random-unit-vector []
  (let [x (- (rand) 0.5)
        y (- (rand) 0.5)
        mag-f (rand)
        mag (js/Math.sqrt (+ (* x x) (* y y)))]
    (if (zero? mag)
      [1 0]
      [(* mag-f (/ x mag)) (* mag-f (/ y mag))])))

(defn- update-bullets [bullets delta-ms time-ms]
  (->> bullets
       (keep (fn [b]
               (when (and (< time-ms (:die-time b))
                          (> (:x b) 0)
                          (> (:y b) 0)
                          (< (:x b) e/GAME-WIDTH)
                          (< (:y b) e/GAME-HEIGHT))
                 (-> b
                     (update :x + (* 0.8 delta-ms (:dx b)))
                     (update :y + (* 0.8 delta-ms (:dy b)))
                     (assoc :opacity (min 1.0 (/ (- (:die-time b)
                                                    time-ms)
                                                 100)))))))
       vec))

(defn- do-movement [v delta-ms speed keystate inc-key dec-key n x]
  ;; if both opposing keys are down, don't do anything
  (if (and (inc-key keystate)
           (dec-key keystate))
    v
    (min x (max n
                ;; depending on whether inc key is pressed or dec key is pressed we use different ops
                (cond
                  (inc-key keystate) (+ v (* delta-ms speed))
                  (dec-key keystate) (- v (* delta-ms speed))
                  :else v)))))

(defn tap [v]
  (println v)
  v)

(defn gen-explosion [x y]
  (let [pcount (+ 20 (rand-int 20))]
    (repeatedly pcount
                (fn []
                  (let [[dx dy] (random-unit-vector)]
                    {:x x, :y y, :dx (* 0.4 dx) , :dy (* 0.4 dy), :size (+ 3 (rand-int 10))
                     :ttl (+ 1000 (rand-int 2000))})))))


(defrecord Explosions [state]
  e/IProvidesRects
  (get-rects [_]
    (mapv (fn [p]
            {:x (:x p)
             :y (:y p)
             :w (:size p)
             :h (:size p)
             :col "white"})
          (:particles @state)))
  e/IListensToCollisions
  (collisions [_ cols]
  ;; if a player collides with an enemy then spawn explosion
    (when (seq cols)
      ;; collect all new explosions this frame, any player <-> enemy
      ;; or bullet <-> enemy needs update
      (let [new-exps (loop [[[a b] & rs] cols
                            exps '()]
                       (let [exp-set #{(:tag a) (:tag b)}
                             new-explosions (cond
                                              (= exp-set #{:player :enemy})
                                              (concat (gen-explosion (:x a) (:y a))
                                                      (gen-explosion (:x b) (:y b)))

                                              (= exp-set #{:enemy :bullet})
                                              (gen-explosion (if (= :enemy (:tag a)) (:x a) (:x b))

                                                             (if (= :enemy (:tag a)) (:y a) (:y b))))]

                         (if (empty? rs)
                           (concat exps new-explosions)
                           (recur rs (concat exps new-explosions)))))]
        (when (seq new-exps)
          (a/play! (rand-nth ["exp1" "exp2" "exp3"]))
          (swap! state update :particles concat new-exps)))))
  e/INeedsUpdate
  (update [_ delta-ms time-ms _]
    (swap! state
           update :particles (fn [ps]
                               (vec
                                (keep (fn [p]
                                        (when (pos? (:ttl p))
                                                ;; this particle is still alive update
                                          (-> p
                                              (update :x + (* (:dx p) delta-ms 0.8))
                                              (update :y + (* (:dy p) delta-ms 0.8))
                                              (update :ttl - delta-ms)
                                              (update :size dampen-to-zero delta-ms))))
                                      ps))))))

(defn make-explosions []
  (Explosions. (atom {:particles []})))


(defn- scatter [v dev]
  (+ v (- (* 2 dev (rand)) dev)))

(defn- handle-removal [curr-enemies hit-enemies]
  ;; when certain enemies are hit, they get destroyed but
  ;; they spawn mor
  (let [new-enemies (mapcat (fn [e]
                              (case (:type e)
                                :cruiser (repeatedly 8 #(w/make-enemy :transport
                                                                      (w/random-id)
                                                                      (scatter (:x e) 20)
                                                                      (scatter (:y e) 20)
                                                                      -1.1
                                                                      (scatter 0 0.5)))
                                :transport (repeatedly 4 #(w/make-enemy :drone
                                                                        (w/random-id)
                                                                        (scatter (:x e) 20)
                                                                        (scatter (:y e) 20)
                                                                        -1.5
                                                                        (scatter 0 0.5)))
                                ;; no further spawning
                                nil))
                            hit-enemies)
        ids (set (map :id hit-enemies))]
    (->> curr-enemies
         (remove (comp ids :id))
         (concat new-enemies))))

(defn- spawn-enemies [curr-enemies time-ms]
  (concat curr-enemies
          (w/next-enemies 0 time-ms)))

(defrecord Enemies [state]
  e/IProvidesRects
  (get-rects [_]
    (mapv (fn [e]
            {:x (:x e)
             :y (:y e)
             :w (:w e)
             :h (:h e)
             :col (case (:type e)
                    :cruiser  "#0082C8"
                    :transport "#EE902B"
                    "red")})
          (:enemies @state)))

  e/INeedsUpdate
  (update [_ delta-ms time-ms _]
    (swap! state
           (fn [st]
             (cond-> st
               :always
               (update :enemies
                       (fn [en]
                         (vec (keep (fn [e]
                                      (when (> (:x e) -50)
                                        (-> e
                                            (update :x + (* (:dx e -1) delta-ms 0.1))
                                            (update :y + (* (:dy e 0) delta-ms 0.1))
                                            #_(update :dy dampen-to-zero))))
                                    en))))

               (> time-ms (:time-to-spawn-enemy st))
               (->
                (update :enemies spawn-enemies time-ms)
                (update :next-enemy-id + 10)
                (assoc :time-to-spawn-enemy (+ time-ms 1000)))))))

  e/IListensToCollisions
  (collisions [_ cols]
  ;; if any of the enemies are colliding, then remove them
    (let [hit-items (keep (fn [[a b]]
                               ;; we need only player vs enemy and bullet vs enemy hits here
                            (when-not (= (:tag a) (:tag b) :enemy)
                                 ;; pick out the enemy
                              (if (= :enemy (:tag a)) a b)))
                          cols)]
      (when-let [hi (seq hit-items)]
        (swap! state
               update :enemies
               handle-removal hi))))

  e/IDoesCollision
  (get-collision-rects [_]
    (:enemies @state)))

(defn make-enemies []
  (Enemies. (atom {:enemies []})))

(defrecord Player [state]
  e/IProvidesRects
  (get-rects [_]
    (let [{:keys [x y recoil thrusting alive?]} @state]
      (into
      ;; if we're alive then render the ship, otherwise just the bullets
       (if alive?
         [{:x (- x 40)
           :y (- y 10)
           :w 40
           :h 20
           :col "#7B337D"}

         ;; the gun
          {:x (- x (* 10 recoil) 2) :y (- y 5) :w 20 :h 10 :col "white"}

         ;; propulsion
          {:x (- x 46) :y (+ y 1) :w (if thrusting 4 0) :h 8 :col "#79D5F3"}
          {:x (- x 46) :y (- y 9) :w (if thrusting 4 0) :h 8 :col "#79D5F3"}]
         [])

        ;; if there any bullets to render, do so now
       (map (fn [b]
              {:x (- (:x b) 5)
               :y (- (:y b) 3)
               :w 10
               :h 6
               :col (str "rgba(" (:base-color b) "," (:opacity b) ")")})
            (:bullets @state)))))

  e/IListensToCollisions
  (collisions [_ cols]
    ;; did we collide with anithing?
    (let [has-collision? (some (fn [[a b]]
                                 (or
                                  (and (= (:tag a) :player) (= (:tag b) :enemy))
                                  (and (= (:tag b) :player) (= (:tag a) :enemy))))
                               cols)]
      (when has-collision?
        (swap! state  assoc :alive? false))))

  e/IDoesCollision
  (get-collision-rects [_]
    (let [{:keys [x y alive? bullets]} @state]
      (into
       (if alive?
         [{:x (- x 40)
           :y (- y 10)
           :w 60
           :h 20
           :tag :player}]
         [])
       (map #(assoc % :tag :bullet) bullets))))

  e/INeedsUpdate
  (update [_ delta-ms time-ms keystate]
    ;; if the user is holding the fire key down, lets fire a few rounds
    (let [st @state
          fire-rate (or (:fire-rate st) 200)
          should-fire? (and (:alive? st)
                            (:fire keystate)
                            (> (- time-ms (:last-fire-time st))
                               fire-rate))
          {:keys [x y]} @state]
      (swap! state
             (fn [{:keys [alive?] :as st}]
               (cond-> st
                 ;; always update bullets as well as any recoil dampening
                 :always (update :bullets update-bullets delta-ms time-ms)
                 :always (update :recoil dampen-to-zero delta-ms)

                 ;; if we should fire then add a bullet and
                 ;; update the vars we keep for tracking fire rate
                 should-fire?
                 (->
                  (assoc :last-fire-time time-ms :recoil 1.0)
                  (update :bullets conj {:x (+ x 20)
                                         :y y
                                         :dx 1.0
                                         :dy (* 0.2 (- 0.5 (rand)))
                                         :spawn-time time-ms
                                         :base-color (if (> (rand) 0.8)
                                                       "247,247,0"
                                                       "201,50,222")
                                         :die-time (+ time-ms (+ 100 (* 500 (rand))))}))

                 (keystate :fire)
                 (update :fire-rate #(max 10 (- % (* 0.01 delta-ms))))

                 (not (keystate :fire))
                 (assoc :fire-rate 120)

                 ;; update y direction movement
                 alive?
                 (update :y do-movement delta-ms (:speed st) keystate :down :up
                         20 (- e/GAME-HEIGHT 20))

                 ;; update x direction movement
                 alive?
                 (update :x do-movement delta-ms (:speed st) keystate :right :left
                         50 (- e/GAME-WIDTH 50))

                 ;; update thrusting (to show engines firing)
                 alive?
                 (assoc :thrusting (some keystate [:up :down :left :right])))))
      (when should-fire?
        (a/play! "fire")))))

(defn make-player []
  (Player. (atom {:x (quot e/GAME-WIDTH 2)
                  :y (quot e/GAME-HEIGHT 2)
                  :fire-rate 120
                  :speed 0.4
                  :recoil 0
                  :alive? true})))


(defn ^:export ^:dev/after-load start []
  (e/init!)
  (a/init!)

  (e/register-game-object! (make-starfield 100 e/GAME-WIDTH e/GAME-HEIGHT))
  (e/register-game-object! (make-explosions))
  (e/register-game-object! (make-enemies))
  (e/register-game-object! (make-player))

  (a/start-bg-music!))

(defn ^:export ^:dev/before-load stop []
  (a/stop!)
  (e/stop!))
