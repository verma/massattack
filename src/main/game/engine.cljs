(ns game.engine
  "Basic engine work"
  (:refer-clojure :exclude [update])
  (:require
   [goog.events :as gevents]
   [goog.object :as gobject]))

(def GAME-WIDTH 720)
(def GAME-HEIGHT 500)


(def *game-state (atom {:canvas nil
                        :game-objects []
                        :release-events-fn nil
                        :key-states {}}))

(defprotocol IProvidesRects
  (get-rects [_]))

(defprotocol IDoesCollision
  (get-collision-rects [_]))

(defprotocol INeedsUpdate
  (update [_ delta-ms time-ms keystates]))

(defprotocol IListensToCollisions
  (collisions [_ cols]))

(defn- key-state! [^js/Event e up-or-down keycode]
  ;; only certain key codes are recognized
  (when (contains? #{70 40 65 38 68 32 83 87 39 37} keycode)
    (.preventDefault e)
    (.stopPropagation e))

  (when-let [k (case keycode
                 (40 83) :down
                 (38 87) :up
                 (70 32) :fire
                 (39 68) :right
                 (37 65) :left
                 nil)]
    (swap! *game-state assoc-in [:key-states k] (if (= :up up-or-down) false true))))

(declare step!)
(defn init! []
  (let [canvas (doto (js/document.createElement "canvas")
                 (gobject/set "width" GAME-WIDTH)
                 (gobject/set "height" GAME-HEIGHT))
        down (gevents/listen js/window "keydown" #(key-state! % :down (.-keyCode %)))
        up (gevents/listen js/window "keyup" #(key-state! % :up (.-keyCode %)))]
    (.. js/document.body (appendChild canvas))

    (reset! *game-state
            {:game-objects []
             :canvas canvas
             :key-states {}
             :release-events-fn (fn []
                                  (gevents/unlistenByKey down)
                                  (gevents/unlistenByKey up))})

    ;; start game
    (step!)))

(defn register-game-object! [p]
  (swap! *game-state cljs.core/update :game-objects conj p))

(defonce *last-time (atom nil))
(defonce *time-since-start (atom 0))

(defn- rects-collide? [r1 r2]
  (and
   (< (:x r1) (+ (:x r2) (:w r2)))
   (> (+ (:x r1) (:w r1)) (:x r2))
   (< (:y r1) (+ (:y r2) (:h r2)))
   (> (+ (:y r1) (:h r1)) (:y r2))))

(defn- colliding-pairs [rects]
  (when (seq rects)
    (loop [[r & rs] rects
           sofar (transient [])]
      (if-let [s (seq rs)]
        (recur rs
               (reduce (fn [acc rr]
                         (if (rects-collide? r rr)
                           (conj! acc [r rr])
                           acc))
                       sofar
                       s))
        (persistent! sofar)))))


(defn- update! []
  (let [now (js/performance.now)
        [old _] (reset-vals! *last-time now)]
    (when old
      (let [dt (- now old)
            gos (:game-objects @*game-state)]
        (swap! *time-since-start + dt)

        ;; any game objects needing update
        (doseq [go gos]
          (when (satisfies? INeedsUpdate go)
            (update go dt @*time-since-start (:key-states @*game-state))))

        ;; collision checks, get collision rects for all entities and then
        ;; collide them
        (let [all-coll-rects (mapcat (fn [go]
                                       (when (satisfies? IDoesCollision go)
                                         (get-collision-rects go)))
                                     gos)
                                     ;; find colliding rects
              colliding (colliding-pairs all-coll-rects)]
                          ;; if there are any collissions notifiy all entities about them
          (when-let [ss (seq colliding)]
            (doseq [go gos]
              (when (satisfies? IListensToCollisions go)
                (collisions go ss)))))))))

(defn- dump-rects-to-ctx! [^js/Canvas ctx rects]
  (doseq [{:keys [x y w h col]} rects]
    (gobject/set ctx "fillStyle" col)
    (.fillRect ctx x y w h)))

(defn- render! []
  (let [st @*game-state]
    (when-let [^js/Canvas c (:canvas st)]
      (when-let [ctx (.getContext c "2d")]
        (doto ctx
          (gobject/set "fillStyle" "black")
          (.fillRect 0 0 GAME-WIDTH GAME-HEIGHT))

        (doseq [go (:game-objects st)]
          (when (satisfies? IProvidesRects go)
            (dump-rects-to-ctx! ctx (get-rects go))))))))

(defn- step! []
  (js/requestAnimationFrame
   (fn []
     (update!)
     (render!)
     (step!))))

(defn stop! []
  (let [[st _] (reset-vals! *game-state nil)]
    (when-let [c (:canvas st)]
      (.remove c))
    (when-let [f (:release-events-fn st)]
      (f))))
