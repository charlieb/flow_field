(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 120)
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
  {:x (q/random w) :y (q/random h) :vx 0.0 :vy 0.0 :history '()})

; MAP FUNCTIONS
(defn pure-perlin [x y]
  (let [noise_scl 0.15]
    (mod (* 12.5 (q/noise (* x noise_scl)  (* y noise_scl))) 6.28)))
(defn sin-sin-perlin [x y]
  (let [noise_scl 0.1
        sin_scl 0.5]
    (+ (* 0.5 (mod (* 12.5 (q/noise (* x noise_scl)  (* y noise_scl))) 6.28))
       (* (/ 6.28 4) (+ 2 (q/sin (* x sin_scl)) (q/sin (* y sin_scl)))))))
(defn sin-sin [x y]
  (let [sin_scl 0.5]
    (* (/ 6.28 4) 
       (+ 2 (+ (q/sin (* x sin_scl)) (q/sin (* y sin_scl)))))))
(defn sinxsin [x y]
  (let [sin_scl 0.5]
    (* (/ 6.28 4) ;; not right but gives nice errrm labial effect
       (+ 1 (* (q/sin (* x sin_scl)) (q/sin (* y sin_scl)))))))

(defn sinxsin-perlin [x y]
  (let [noise_scl 0.2
        sin_scl 0.5]
    (+ (* 0.3 (mod (* 12.5 (q/noise (* x noise_scl)  (* y noise_scl))) 6.28))
       (* (/ 6.28 4) ;; not right but gives nice errrm labial effect
          (+ 1 (* (q/sin (* x sin_scl)) (q/sin (* y sin_scl))))))))

(defn sin-sin2 [x y]
  (let [xscls '(0.5 0.1)
        yscls '(0.55 0.15)
        len (+ (count xscls) (count yscls))]
    (* (/ 6.28 (* 2 len))
       (+ len (reduce + (concat (map (fn [xscl] (q/sin (* x xscl))) xscls)
                                (map (fn [yscl] (q/sin (* y yscl))) yscls)))))))

;---------------------------------
(defn xya-from [p angle d]
  {:a angle
   :x (+ (:x p) (* (q/cos angle) d))
   :y (+ (:y p) (* (q/sin angle) d))})

(defn find-contour [p f target]
  "point, map function, target value"
  (let [nsamples 50
        dist 1.0
        search (fn [high low]
                  (->> (range nsamples) 
                       (map (fn [n] (+ low (* n (/ (- high low) (- nsamples 1))))))
                       (map (fn [a] (xya-from p a dist)))
                       (apply min-key (fn [p] (Math/abs (float (- (f (:x p) (:y p)) target)))))))
        ]
    (search (+ (:a p) 3.14) (- (:a p) 3.14))))


;---------------------------------
(defn update-state [state]
  (let [x 50
        y 50
        scl 10 
        sx (* x scl)
        sy (* y scl)

        step 0.01
        regen-noise false
        regen-field false
        nparts 100
        max-len 10

        is-dead (fn [p] (or (hits-edge (:x p) (:y p) sx sy)
                            (> (count (:history p)) max-len)))

        rot (if (nil? state) 0.0 (+ (:rot state) step))
        offset (if (or (nil? state) regen-noise)
                 (map (fn [i] 
                        (let [{i :x j :y} (to-xy i x)]
                          ;(sin-sin i j)
                          (sin-sin2 i j)
                          ;(pure-perlin i j)
                          ;(sinxsin i j)
                          ;(sinxsin-perlin i j)
                          ))
                      (range (* x y)))
                 (:offset state))
        dead-parts (if (nil? state) '()
                     (let [new-dead (map :history (filter is-dead (:parts state)))]
                       (if (nil? new-dead)
                         (:dead-parts state)
                         (concat new-dead (:dead-parts state))
                           )))
        parts (if (or (nil? state) (not (= nparts (count (:parts state)))))
                (repeatedly nparts (fn [] (new-part sx sy)))
                (map (fn [p]
                       (if (is-dead p)
                         (new-part sx sy)
                       (let [px (int (/ (:x p) scl))
                             py (int (/ (:y p) scl))
                             acc (nth (:field state) (from-xy px py x))]
                         {:x (+ (:x p) (:vx p))
                          :y (+ (:y p) (:vy p))
                          :vx (+ (* (:vx p) 0.75) (* (:x acc) 0.25))
                          :vy (+ (* (:vy p) 0.75) (* (:y acc) 0.25))
                          :history (if (zero? (mod (:iterations state) 10))
                                     (conj (:history p) {:x (:x p) :y (:y p)})
                                     (:history p))})))
                     (:parts state)))
        field (if (or (nil? state) regen-field)
                (map (fn [off] {:x (q/cos (+ off rot)) 
                                :y (q/sin (+ off rot))}) 
                      offset)
                (:field state))
        iterations (if (nil? state) 1 (+ (:iterations state) 1))
        ]
    {:x x
     :y y
     :scl scl
     :rot rot
     :offset offset
     :parts parts
     :dead-parts dead-parts
     :field field
     :iterations iterations
     }))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ; Set color.
  (if (and (not (nil? state)) (zero? (mod (:iterations state) 200)))
    (do
      (q/background 240)
      (q/stroke-weight 1)
      (q/stroke 0 0 0 55)
      ; Field lines
      (when false 
        (doseq [[fi field] (map-indexed list (:field state))]
          (let [{i :x j :y} (to-xy fi (:x state))]
            (q/stroke-weight 5)
            (q/point (* i (:scl state)) (* j (:scl state)))
            (q/stroke-weight 1)
            (q/line (* i (:scl state))
                    (* j (:scl state))
                    (* (+ i (:x field)) (:scl state))
                    (* (+ j (:y field)) (:scl state))))))
      (when true
        ;(q/stroke-weight 5)
        (q/stroke 128 0 0 55)
        (doseq [p (:parts state)]
          ;(q/point (:x p) (:y p))
          (doseq [[h hp] (map list (:history p) (rest (:history p)))]
            (q/line (:x h) (:y h) (:x hp) (:y hp)))
          ))

      (when true ; Dead part lines
        (doseq [p (:dead-parts state)]
          (doseq [[h hp] (map list p (rest p))]
            (q/line (:x h) (:y h) (:x hp) (:y hp)))))))

   (when (and (not (nil? state)) (zero? (mod (:iterations state) 30)))
    (q/save "hairy3.png"))

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
