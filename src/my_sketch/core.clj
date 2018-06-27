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
  {:x (q/random w) :y (q/random h) :a 0.0 :vx 0.0 :vy 0.0 :history '()})

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
  (let [sin_scl 0.15]
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

(defn sin-sin-sq [x y]
  (let [sin_scl 0.1]
    (* (/ 6.28 4) 
       (+ 2 (+ (q/sin (* x x sin_scl)) (q/sin (* y y sin_scl)))))))


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
        dist 10.0
        search (fn [high low]
                 (let [res
                       (->> (range nsamples) 
                            (map (fn [n] (+ low (* n (/ (- high low) (- nsamples 1))))))
                            (map (fn [a] (xya-from p a dist)))
                            ;(apply min-key (fn [p] (Math/abs (float (- (f (:x p) (:y p)) target)))))
                            )]
                   ;(doseq [r res] (println r (f (:x r) (:y r)) (Math/abs (float (- (f (:x r) (:y r)) target)))))
                   (apply min-key (fn [p] (Math/abs (float (- (f (:x p) (:y p)) target)))) res)))
                      ]
    (search (+ (:a p) (* 3.1415 (/ (- nsamples 1) nsamples))) 
            (- (:a p) (* 3.1415 (/ (- nsamples 1) nsamples))))))

(defn follow-contour [p f]
  (doseq [pt (take 5 (iterate (fn [pi] (find-contour pi f (f (:x p) (:y p)))) p))]
    (println '---------')
    (println pt (f (:x pt) (:y pt)))))


;---------------------------------
(defn is-particle-dead [p w h]
  (let [max-len 10]
    (or (hits-edge (:x p) (:y p) w h)
        (> (count (:history p)) max-len))))

(defn update-contour-particles-state [state]
  (let [x (:x state)
        y (:y state)
        scl (:scl state) 
        sx (* x scl)
        sy (* y scl)

        scaled-fn (fn [i j] ((:map-fn state)
                             (/ i scl)
                             (/ j scl)))
        new-part-target (fn [] (let [p (new-part sx sy)]
                                 (assoc p :target (scaled-fn (:x p) (:y p)))))
        nparts 100
        ]
    (assoc state
           :parts (if (or (not (contains? state :parts)) (not (= nparts (count (:parts state)))))
                    (repeatedly nparts new-part-target)
                    (map (fn [p]
                           (if (is-particle-dead p sx sy)
                             (new-part-target)
                             (assoc (find-contour p scaled-fn (:target p))
                                    :history (if (zero? (mod (:iterations state) 10))
                                               (conj (:history p) {:x (:x p) :y (:y p)})
                                               (:history p)))))
                         (:parts state)))
           )))

(defn update-flow-particles-state [state]
  (let [x (:x state)
        y (:y state)
        scl (:scl state) 
        sx (* x scl)
        sy (* y scl)

        nparts 200

        field (if (contains? state :field)
                (:field state)
                (map (fn [i]
                        (let [{i :x j :y} (to-xy i x)
                              value ((:map-fn state) i j)]
                          {:x (q/cos value) 
                           :y (q/sin value)}))
                     (range (* x y))))
        ]
    (assoc state
           :parts (if (or (not (contains? state :parts)) (not (= nparts (count (:parts state)))))
                    (repeatedly nparts (fn [] (new-part sx sy)))
                    (map (fn [p]
                           (if (is-particle-dead p sx sy)
                             (new-part sx sy)
                             (let [px (int (/ (:x p) scl))
                                   py (int (/ (:y p) scl))
                                   acc (nth field (from-xy px py x))]
                               {:x (+ (:x p) (:vx p))
                                :y (+ (:y p) (:vy p))
                                :vx (+ (* (:vx p) 0.75) (* (:x acc) 0.25))
                                :vy (+ (* (:vy p) 0.75) (* (:y acc) 0.25))
                                :history (if (zero? (mod (:iterations state) 10))
                                           (conj (:history p) {:x (:x p) :y (:y p)})
                                           (:history p))})))
                         (:parts state)))
           :field field)))

(defn update-state [state]
  (let [x 50
        y 50
        scl 10 
        sx (* x scl)
        sy (* y scl)

        map-fn (fn [i j] 
                 (distance i j)
                 ;(gradient i j)
                 ;(+ (* 20 (circle i j)) (sin-sin-sq i j))
                 ;(+ (* 10 (circle i j)) (sin-sin i j))
                 ;(sin-sin-sq i j)
                 ;(* (sin-sin i j) (circle i j))
                 ;(* (pure-perlin i j) (circle i j))
                 ;(/ (circle i j) (+ i 1))
                 ;(sin-sin i j)
                 ;(sin-sin2 i j)
                 ;(pure-perlin i j)
                 ;(sinxsin i j)
                 ;(sinxsin-perlin i j)
                 )

        dead-parts (if (nil? state) '()
                     (let [new-dead (map :history (filter (fn [p] (is-particle-dead p sx sy)) (:parts state)))]
                       (if (nil? new-dead)
                         (:dead-parts state)
                         (concat new-dead (:dead-parts state))
                           )))

        iterations (if (nil? state) 1 (+ (:iterations state) 1))
        ]
    (assoc state
           :x x
           :y y
           :scl scl
           :map-fn map-fn
           :dead-parts dead-parts
           :iterations iterations
           )))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  ; Set color.
  (if (and (not (nil? state)) (zero? (mod (:iterations state) 100)))
    (do
      (println (q/current-frame-rate))
      (q/background 240)
      (q/stroke-weight 1)
      (q/stroke 0 0 0 128)
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


  (when (and false (not (nil? state)) (zero? (mod (:iterations state) 30)))
    (q/save "hairy.png"))

  )

(defn -main [& args]
  (q/defsketch my-sketch
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update (fn [state] (-> state
                            update-state
                            ;update-flow-particles-state
                            update-contour-particles-state
                            ))
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
