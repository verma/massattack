(ns game.waves
  (:require [game.engine :as e]))


(def waves
  [["o.....o.............o.................*..........."
    ".o.o.o.......o.......o......................*..*.."
    ".o.o.o......o.o.......o......*........*..........."
    "o.....o....o...o.......x.oo..*..x.*....xxxx......."
    ".o.o.o......o.o.......o......*........*..........."
    ".o.o.o.......o.......o......................*..*.."
    "o.....o.............o.................*..........."]])

(defn make-enemy
  ([enemy-type id x y]
   (make-enemy enemy-type id x y -1 0))
  ([enemy-type id x y dx dy]
   (->
    (case enemy-type
      :cruiser {:w 50 :h 10}
      :transport {:w 20 :h 20}
     ;; default drone class
      {:w 10 :h 10})
    (assoc :x x :y y
           :dx dx :dy dy
           :type (or enemy-type :drone)
           :tag :enemy
           :id id))))

(defn random-id []
  (subs
   (.toFixed (js/Math.random) 16)
   2))

(defn next-enemies
  "Returns the sequence of enemies to generate for the given time and wave"
  [wave-number time-since-wave-start-ms]
  (let [lines (nth waves wave-number)
        index (mod (js/Math.floor (* 0.001 time-since-wave-start-ms))
                   (count (ffirst waves)))]
    (keep-indexed (fn [idx l]
                    (let [y (+ 20 (* idx (/ (- e/GAME-HEIGHT 20) 7)))]
                      (case (nth l index)
                        \o (make-enemy :drone (random-id) (+ e/GAME-WIDTH 1) y)
                        \x (make-enemy :transport (random-id) (+ e/GAME-WIDTH 1) y)
                        \* (make-enemy :cruiser (random-id) (+ e/GAME-WIDTH 1) y)
                        nil)))
                  lines)))