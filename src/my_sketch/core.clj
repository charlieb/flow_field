(ns my-sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(set! *warn-on-reflection* true)


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
  {:x (q/random (float w)) :y (q/random (float h)) :a 0.0 :vx 0.0 :vy 0.0 :history '() :target 1})

; MAP FUNCTIONS
(defn pure-perlin [x y noise_scl]
    (q/noise (* x noise_scl)  (* y noise_scl)))
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
  (let [xscls '(0.5 0.1 0.073)
        yscls '(0.55 0.15)
        len (+ (count xscls) (count yscls))]
    (* (/ 6.28 (* 2 len))
       (+ len (reduce + (concat (map (fn [xscl] (q/sin (* x xscl))) xscls)
                                (map (fn [yscl] (q/sin (* y yscl))) yscls)))))))
(defn circle [x y]
  (let [cx 75
        cy 75]
    (- (q/atan2 (- cy y) (- cx x))
       1.6)))

(defn sin-sin-sq [x y scl]
  (* (/ 6.28 4) 
     (+ 2 (+ (q/sin (* x x scl)) (q/sin (* y y scl))))))

(defn perlin-sin [x y pscl sscl]
   (+ (q/sin (* x sscl)) 
      (q/noise (* x pscl) (* y pscl))))




(defn gradient [x y]
  (/ x 10.))
(defn distance [x y]
  (+ (* x x) (* y y)))

;---------------------------------
(defn xya-from [p angle d]
  (let [r 
  (assoc p
         :a angle
         :x (+ (:x p) (* (q/cos angle) d))
         :y (+ (:y p) (* (q/sin angle) d)))]
    ;(println p r)
    r))

(defn find-contour [p f target]
  "point, map function, target value"
  (let [nsamples 50
        dist 1.0
        half-look 2.0;3.1415
        search (fn [high low]
                 (->> (range nsamples) 
                      (map (fn [n] (+ low (* n (/ (- high low) (- nsamples 1))))))
                      (map (fn [a] (xya-from p a dist)))
                      (apply min-key (fn [p] (Math/abs (float (- (f (:x p) (:y p)) target)))))))
                      ]
    (search (+ (:a p) half-look)
            (- (:a p) half-look))))

(defn follow-contour [p f]
  (doseq [pt (take 5 (iterate (fn [pi] (find-contour pi f (f (:x p) (:y p)))) p))]
    (println '---------')
    (println pt (f (:x pt) (:y pt)))))


;---------------------------------
(defn is-particle-dead [p w h]
  (let [max-len 20]
    (or (hits-edge (:x p) (:y p) w h)
        (> (count (:history p)) max-len))))

(defn call-map [i j state]
  (apply (get-in state '(:parameters :map-fn))
         (concat (list i j) 
                 (get-in state '(:parameters :map-fn-parameters)))))

;  :contour {:half-look 2.0
;              :step 1.0
;              :nsamples 50
;              :allow-multiples-of 0.5
;              :max-error 0.01
   
(defn update-contour-particles-state [state]
  (let [x (:x state)
        y (:y state)
        scl (:scl state) 
        sx (* x scl)
        sy (* y scl)

        scaled-fn (fn [i j] (call-map (/ i scl) (/ j scl) state))
        new-part-target 
        (fn [] (let [p (new-part sx sy)]
                 (assoc p :target 
                        (let [v (scaled-fn (:x p) (:y p))]
                          (- v (mod v (get-in state '(:parameters :contour :allow-multiples-of))))))))
        ]
    (assoc state
           :parts
           (if (empty? (state :parts))
             (repeatedly (:nparts state) new-part-target)
             (map (fn [p]
                    (if (is-particle-dead p sx sy)
                      (new-part-target)
                      (assoc (find-contour p scaled-fn (:target p))
                             :history (if (and (zero? (mod (:iterations state)
                                                           (get-in state '(:parameters :history-rate))))
                                               (< (Math/abs (float
                                                              (- (scaled-fn (:x p) (:y p))
                                                                 (:target p))))
                                                  (get-in state '(:parameters :contour :max-error))))
                                        (conj (:history p) {:x (:x p) :y (:y p) :target (:target p)})
                                        (:history p)))))
                  (:parts state)))
           )))

(defn update-flow-particles-state [state]
  (let [x (:x state)
        y (:y state)
        scl (:scl state) 
        sx (* x scl)
        sy (* y scl)

        field (if (contains? state :field)
                (:field state)
                (map (fn [i]
                       (let [{i :x j :y} (to-xy i x)
                             value (call-map i j state)]
                         {:x (q/cos value) 
                          :y (q/sin value)}))
                     (range (* x y))))
        ]
    (assoc state
           :parts (if (empty? (:parts state))
                    (repeatedly (:nparts state) (fn [] (new-part sx sy)))
                    (map (fn [p]
                           (if (is-particle-dead p sx sy)
                             (new-part sx sy)
                             (let [px (int (/ (:x p) scl))
                                   py (int (/ (:y p) scl))
                                   acc (nth field (from-xy px py x))]
                               (assoc
                                 p
                                 :x (+ (:x p) (:vx p))
                                 :y (+ (:y p) (:vy p))
                                 :vx (+ (* (:vx p) 0.75) (* (:x acc) 0.25))
                                 :vy (+ (* (:vy p) 0.75) (* (:y acc) 0.25))
                                 :history (if (zero? (mod (:iterations state) 
                                                          (get-in state '(:parameters :history-rate))))
                                            (conj (:history p) {:x (:x p) :y (:y p) :target 1})
                                            (:history p))))))
                         (:parts state)))
           :field field)))

(defn setup [parameters]
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 120)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb 1.0)
  ; setup function returns initial state.
  {
   :x 50
   :y 50
   :scl 10
   :nparts 200
   :dead-parts '()
   :iterations 0
   :parameters parameters
   :parts '()
   }
  )

(defn update-state [state]
  (let [sx (* (:x state) (:scl state))
        sy (* (:y state) (:scl state))

        map-fn (fn [i j] 
                 ;(distance i j)
                 ;(gradient i j)
                 ;(+ (* 20 (circle i j)) (sin-sin-sq i j))
                 ;(+ (* 1 (circle i j)) (sin-sin i j))
                 ;(sin-sin-sq i j)
                 ;
                 ;(* (sin-sin i j) (circle i j))
                 ;(* (pure-perlin i j) (circle i j))
                 ;(/ (circle i j) (+ i 1))
                 (sin-sin i j)
                 ;(sin-sin2 i j)
                 ;(pure-perlin i j)
                 ;(sinxsin i j)
                 ;(sinxsin-perlin i j)
                 )

        ]
    (assoc state
           :dead-parts
           (let [new-dead (filter (complement empty?) 
                                  (map :history (filter (fn [p] (is-particle-dead p sx sy))
                                                        (:parts state))))]
             (if (nil? new-dead)
               (:dead-parts state)
               (concat new-dead (:dead-parts state))))
           
           :iterations (+ (:iterations state) 1))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ; Set color.
  (println 'draw (:iterations state))
  (if (and (not (empty? (:parts state)))
           (zero? (mod (:iterations state) 200)))
    (do
      (println (q/current-frame-rate))
      (q/background 0.8)
      (q/stroke-weight 1)
      (q/stroke 0 0 0 0.5)
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

      (q/stroke 0.5 0 0 0.2)

      (let [values (map :target
                        (concat (:parts state)
                                (map first (:dead-parts state))))
            hue1 0.1
            hue2 0.9
            high (apply max values)
            low  (apply min values)
            fac (fn [v] (+ hue1 (* (- hue2 hue1) (/ (- v low) high))))]
        (when true
          ;(q/stroke-weight 5)
          (doseq [p (:parts state)]
            (q/stroke (fac (:target p)) 1.0 0.8 0.2)
            ;(q/point (:x p) (:y p))
            (doseq [[h hp] (map list (:history p) (rest (:history p)))]
              (q/line (:x h) (:y h) (:x hp) (:y hp)))
            ))

        (when true ; Dead part lines
          (doseq [p (:dead-parts state)]
            (q/stroke (fac (:target (first p))) 1.0 0.8 0.2)
            (doseq [[h hp] (map list p (rest p))]
              (q/line (:x h) (:y h) (:x hp) (:y hp))))))))

  (when (and false (not (nil? state)) (zero? (mod (:iterations state) 30)))
    (q/save "hairy.png"))

  )


(def parameters 
  {
   :sin-sin-flow
   {:map-fn sin-sin
    :map-fn-parameters '()
    :update-type :flow
    :history-rate 10
    }

   :sin-sin-sq-flow
   {:map-fn sin-sin-sq
    :map-fn-parameters '(0.005)
    :update-type :flow
    :history-rate 10
    }

   :sin-sin-contours-stepped
   {:map-fn sin-sin
    :map-fn-parameters '()
    :update-type :contour
    :contour {:half-look 2.0
              :step 1.0
              :nsamples 50
              :allow-multiples-of 0.25
              :max-error 0.01
              }
    :history-rate 2
    }

   :sin-sin-sq-contours-stepped
   {:map-fn sin-sin-sq
    :map-fn-parameters '(0.02)
    :update-type :contour
    :contour {:half-look 2.0
              :step 1.0
              :nsamples 50
              :allow-multiples-of 0.25
              :max-error 0.01
              }
    :history-rate 2
    }

   :sin-sin-sq-perlin-contours-stepped
   {:map-fn (fn [i j sscl pscl] 
              (+ (q/sin (* i i sscl))
                 (q/sin (* j j sscl))
                 (* 10. (pure-perlin i j pscl))))
    :map-fn-parameters '(0.05 0.15)
    :update-type :contour
    :contour {:half-look 2.0
              :step 1.0
              :nsamples 50
              :allow-multiples-of 0.05 ; very large because perlin is x10
              :max-error 0.01
              }
    :history-rate 2
    }

   :perlin-contours-stepped
   {:map-fn pure-perlin
    :map-fn-parameters '(0.15)
    :update-type :contour
    :contour {:half-look 2.0
              :step 1.0
              :nsamples 50
              :allow-multiples-of 0.025
              :max-error 0.01
              }
    :history-rate 2
    }

   :perlin-sin-contours
   {:map-fn perlin-sin
    :map-fn-parameters '(0.05 0.5)
    :update-type :contour
    :contour {:half-look 2.0
              :step 1.0
              :nsamples 50
              :allow-multiples-of 0.005
              :max-error 0.005
              }
    :history-rate 2
    }

   :template
   {:map-fn identity
    :map-fn-parameters '()
    :update-type :flow
    :contour {:half-look 3.1415
              :step 1.
              :nsamples 50
              :allow-multiples-of 2
              :max-error 0.01
              }
    :history-rate 10
    }}
  )
(defn run-sketch [selector]
  (q/defsketch my-sketch
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup (fn [] (setup (selector parameters)))
    ; update-state is called on each iteration before draw-state.
    :update (fn [state] (-> state
                            update-state
                            ((fn [state]
                               (case (get-in state '(:parameters :update-type))
                                 :flow (update-flow-particles-state state)
                                 :contour (update-contour-particles-state state))))))
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

(defn -main [& args]
  (run-sketch (first args)))

(deftype V [^float x ^float y])
(deftype Particle [^float x ^float y ^float vx ^float vy ^float r])

(defn dist-sq [^Particle p1 ^Particle p2]
  (let [dx (- (.x p2) (.x p1))
        dy (- (.y p2) (.y p1))]
    (+ (* dx dx) (* dy dy))))
(defn pen-depth [^Particle p1 ^Particle p2]
  (- (+ (.r p1) (.r p2)) (Math/sqrt (dist-sq p1 p2))))
(defn collides? [^Particle p1 ^Particle p2]
  (let [rs (+ (.r p1) (.r p2))]
    (<= (* rs rs) (dist-sq p1 p2))))
(defn mag [^V v] (Math/sqrt (+ (* (.x v) (.x v)) (* (.y v) (.y v)))))
(defn scl [^V v ^double s] (V. (* (.x v) s) (* (.y v) s)))
(defn collision-normal [^Particle p1 ^Particle p2]
  (V. (- (.x p2) (.x p1))
      (- (.y p2) (.y p1))))
(defn rel-vel [^Particle p1 ^Particle p2] true)

