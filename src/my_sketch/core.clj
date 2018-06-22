(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  )

(defn to-xy [i w] {:x (rem i w) :y (quot i w)})
(defn from-xy [x y w] (+ x (* y w)))
(defn wrap [x w] 
  (cond (>= x w) 0.0 
        (< x 0.0) (- w 1)
        :else x))
(defn hits-edge [x y w h]
  (or (>= x w) (< x 0.0)
      (>= y h) (< y 0.0)))
(defn new-part [w h]
  {:x (q/random w) :y (q/random h) :vx 0.0 :vy 0.0})
(defn update-state [state]
  (let [x 20
        y 20
        scl 20
        sx (* x scl)
        sy (* y scl)

        step 0.01
        rotscl 0;.0314
        noise_scl 0.2
        regen-noise false
        regen-field false
        nparts 1500

        rot (if (nil? state) 0.0 (+ (:rot state) step))
        offset (if (or (nil? state) regen-noise)
                 (map (fn [i] 
                        (let [{i :x j :y} (to-xy i x)]
                          (* 6.28 (q/noise (* noise_scl i)  (* noise_scl j)))))
                      (range (* x y)))
                 (:offset state))
        parts (if (or (nil? state) (not (= nparts (count (:parts state)))))
                (repeatedly nparts (fn [] (new-part sx sy)))
                (map (fn [p]
                       (if (hits-edge (:x p) (:y p) sx sy)
                         (new-part sx sy)
                       (let [px (int (/ (:x p) scl))
                             py (int (/ (:y p) scl))
                             acc (nth (:field state) (from-xy px py x))]
                         {:x (+ (:x p) (:vx p))
                          :y (+ (:y p) (:vy p))
                          :vx (+ (* (:vx p) 0.75) (* (:x acc) 0.25))
                          :vy (+ (* (:vy p) 0.75) (* (:y acc) 0.25))})))
                     (:parts state)))
        field (if (or (nil? state) regen-field)
                (map (fn [a off] {:x (q/cos (+ off rot (* a rotscl))) 
                                  :y (q/sin (+ off rot (* a rotscl)))}) 
                     (range (* x y)) offset)
                (:field state))
        iterations (if (nil? state) 1 (+ (:iterations state) 1))
        ]
    {:x x
     :y y
     :scl scl
     :rot rot
     :offset offset
     :parts parts
     :field field
     :iterations iterations
     }))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ; Set color.
  (if (and (not (nil? state)) (zero? (mod (:iterations state) 1)))
    (do
      (q/background 240)
      (q/stroke-weight 1)
      (q/stroke 0 0 0 255)
      (doseq [[fi field] (map-indexed list (:field state))]
        (let [{i :x j :y} (to-xy fi (:x state))]
          (q/line (* i (:scl state))
                  (* j (:scl state))
                  (* (+ i (:x field)) (:scl state))
                  (* (+ j (:y field)) (:scl state)))))
      (q/stroke-weight 5)
      (q/stroke 128 0 0 255)
      (doseq [p (:parts state)] (q/point (:x p) (:y p)))
      ))
  (println (q/current-frame-rate)))

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
