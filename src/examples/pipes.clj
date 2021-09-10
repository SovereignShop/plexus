(ns examples.pipes
  (:require
   [scad-clj.scad :as s]
   [scad-clj.model :as m]
   [scad-paths.core
    :refer [context left right forward up down hull path ->main-model]]))

(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            [(left :angle (/ Math/PI 2)) (left :angle (/ Math/PI 2))]
            [(right :angle (/ Math/PI 2)) (right :angle (/ Math/PI 2))]
            [(forward :length 10) (forward :length 10)]
            [(up) (up)]])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))

;; Or equivalently:


(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (left :angle (/ Math/PI 2))
            (right :angle (/ Math/PI 2))
            (forward :length 10)
            (up)
            (forward :length 20)])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))


;; Hulls

(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (forward :length 20)
            [(context :shape (m/square 20 20)) (context :shape (m/square 16 16))]
            (forward :length 20)
            (hull)
            (forward :length 20)
            [(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (forward :length 20)
            (hull)])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))

;; Branching

(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            [:branch
             [(left)
              (right)
              (forward)]]
            #_[:branch
             [(right)
              (left)
              (forward)]]])
     #_(->main-model)
     #_(s/write-scad))
