(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  )

(defn to-xy [i w] {:x (rem i w) :y (quot i w)})
(defn update-state [state]
  (let [x 50
        y 50
        step 0.01
        scl 10
        rotscl 0;.0314
        noise_scl 0.2
        regen-noise false
        regen-field true

        rot (if (nil? state) 0.0 (+ (:rot state) step))
        offset (if (or (nil? state) regen-noise)
                 (map (fn [i] 
                        (let [{i :x j :y} (to-xy i x)]
                          (* 6.28 (q/noise (* noise_scl i)  (* noise_scl j)))
                          ))
                      (range (* x y)))
                 (:offset state))
        parts (if (nil? state) (iterate (fn [_] {:x (q/random x) :y (q/random y)}) 100) (:parts state))
        field (if (or (nil? state) regen-field)
                (map (fn [a off] {:x (q/cos (+ off rot (* a rotscl))) 
                                  :y (q/sin (+ off rot (* a rotscl)))}) 
                 (range (* x y)) offset)
                (:field state))
        ]
    {:x x
     :y y
     :scl scl
     :rot rot
     :offset offset
     :parts parts
     :field field}))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set color.
  (q/stroke 0 0 0 255)
  (doseq [[fi field] (map-indexed list (:field state))]
    (let [{i :x j :y} (to-xy fi (:x state))]
      (q/line (* i (:scl state))
              (* j (:scl state))
              (* (+ i (:x field)) (:scl state))
              (* (+ j (:y field)) (:scl state)))))
  (println (q/current-frame-rate))
  )

(defn -main [& args]
  (q/defsketch my-sketch
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
