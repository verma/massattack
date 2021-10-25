(ns game.audio
  (:require ["howler" :refer [Howl]]))

(def ^:private sounds (atom {}))


(defn init! []
  (let [loaded (into {}
                     (map (fn [s]
                            [s (Howl.
                                (js-obj "src" (array (str "sounds/" s ".webm")
                                                     (str "sounds/" s ".mp3"))
					;; the fire sound is a bit loud
                                        "volume" (if (= s "fire") 0.4 0.8)))])
                          ["fire" "exp1" "exp2" "exp3"]))]
    (reset! sounds loaded)))

(defn stop! []
  (let [[old _] (reset-vals! sounds {})]
    (when-let [^js s (get old "bg")]
      (.stop s)))
  (reset! sounds {}))

(defn start-bg-music! []
  (let [s (Howl. #js {:src (array "sounds/bg.webm", "sounds/bg.mp3")
                      :html5 true
                      :volume 0.2})]
    (swap! sounds assoc "bg" s)
    (.play s)))

(defn play! [n]
  (when-let [^js p (get @sounds n)]
    (.play p)))