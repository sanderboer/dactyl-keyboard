(ns dactyl-keyboard.lightcycle
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi)
  )


;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.4) ;; Was 14.1, then 14.25
(def keyswitch-width 14.4)
(def sa-profile-key-height 12.7)
(def plate-thickness 4)
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))
(def alps-width 15.6)
(def alps-height 13)
(def alps-notch-width 15.5)
(def alps-notch-height 1)
(def grid-width 20.5)
(def grid-height 22.5)

(def single-plate
     (let [switch-width alps-width
           switch-height alps-height
           wall-thickness 3
           wall-height plate-thickness
           key-grid-height 19.1
           key-grid-width 19.1
           key-frame (->> (cube key-grid-width key-grid-height wall-height ))
           hole (->> (cube switch-width switch-height (* wall-height 2) )
                     )
                          
           ]

     (difference key-frame hole)
       )

     )

(def alps-switch
     (let[switch (->> (cube alps-width alps-height 11.5)
                      (translate [0 0 (- 6.9 (/ 11.5 2))]))
          stem (->> (cube 7.6 4.4 4.78)
                    (translate [0 0 8.542]))
          bl2 (/ 18.5 2)
          m (/ 17 2)
          key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))
          ]
       (union
        (->> switch (color [0.1 0.1 0.1 1]))
       (->> stem (color [0.9 0.9 0.9 1]))
       (->> key-cap
                      (translate [0 0 5])
                      (color [220/255 163/255 163/255 1])))
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 5))
(def rows (range 1 4))
(def α (deg2rad 15))
(def β (deg2rad 5))
(def tent-angle (deg2rad 17))
(def z-offset 13)

(def cap-top-height (+ plate-thickness sa-profile-key-height))

#_(def row-radius (+ (/ (/ (+ mount-height 1/2) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
#_(def column-radius (+ (/ (/ (+ mount-width 2.0) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))

(def row-radius  (/ (/ grid-height 2)
                      (Math/sin (/ α 2)))
                   )
(def column-radius  (/ (/ grid-width 2)
                         (Math/sin (/ β 2)))
                      )

(defn key-place [column row shape
                 & {:keys [stagger] :or {stagger true}}]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset (cond (= stagger true)
                            (cond (= column 2) [0 5 -3.0] ;;was moved -4.5
                                  (>= column 4) [0 -9 5.64]
                                  :else [0 0 0])
                            :else [0 0 0])
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate tent-angle [0 1 0])
         (translate [0 0 z-offset]))))

(defn case-place [column row shape]
  (let [row-placed-shape (->> shape
                              (translate [0 0 (- row-radius)])
                              (rotate (* α (- 2 row)) [1 0 0])
                              (translate [0 0 row-radius]))
        column-offset [0 -4.35 5.64]
        column-angle (* β (- 2 column))
        placed-shape (->> row-placed-shape
                          (translate [0 0 (- column-radius)])
                          (rotate column-angle [0 1 0])
                          (translate [0 0 column-radius])
                          (translate column-offset))]
    (->> placed-shape
         (rotate tent-angle [0 1 0])
         (translate [0 0 z-offset]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; from scratch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def key-holes
  (apply union
         (for [column columns
               row rows]
           (->> single-plate
                (key-place column row)))))

(def key-caps
         (for [column columns
               row rows]
           (->> alps-switch
                (key-place column row))))


;;;;;;;;;;
;; Case ;;
;;;;;;;;;;
;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(def scratch
     (union key-caps
     key-holes)
   )
(spit "things/scratch.scad"
      (write-scad scratch))
