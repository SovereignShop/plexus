(ns examples.pipes
  (:require
   [clj-manifold3d.core :as m]
   [plexus.core
    :refer [result frame left right forward up down hull extrude set branch arc defmodel
            rotate translate segment difference union intersection points export insert
            loft trim-by-plane offset]]))

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20 :to [:body]) (set :curve-radius 20 :to [:mask])

     (left :angle (/ Math/PI 2) :to [:body])
     (left :angle (/ Math/PI 2) :to [:mask])

     (right :angle (/ Math/PI 2) :to [:body])
     (right :angle (/ Math/PI 2) :to [:mask])

     (forward :length 10 :to [:body])
     (forward :length 10 :to [:mask])

     (up :angle (/ Math/PI 2) :to [:body])
     (up :angle (/ Math/PI 2) :to [:mask]))
    (export "test.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; Or equivalently:

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20 :to [:body :mask])

     (left :angle (/ Math/PI 2) :to [:body :mask])
     (right :angle (/ Math/PI 2) :to [:body :mask])
     (forward :length 10 :to [:body :mask])
     (up :angle (/ Math/PI 2) :to [:body :mask]))
    (export "pipes.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; ... Or equivalently:

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20)

     (left :angle (/ Math/PI 2))
     (right :angle (/ Math/PI 2))
     (forward :length 10)
     (up :angle (/ Math/PI 2) :to [:body :mask]))
    (export "pipes.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; Hulls

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20)
     (hull
      (hull
       (forward :length 20)
       (set :cross-section (m/square 20 20 true) :to [:body])
       (set :cross-section (m/square 16 16 true) :to [:mask])
       (forward :length 20))
      (set :cross-section (m/circle 6) :to [:body])
      (set :cross-section (m/circle 4) :to [:mask])
      (forward :length 20)))
    (export "hull.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; Lofts

(-> (extrude
     (result :name :pipes :expr :body)
     (frame :cross-section (m/difference (m/circle 20) (m/circle 18)) :name :body)
     (loft
      (forward :length 1)
      (for [i (range 3)]
        [(translate :x 8)
         (forward :length 20)
         (translate :x -8)
         (forward :length 20)])))
    (export "loft.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; Branching

(def pi|2 (/ Math/PI 2))

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 10)

     (branch :from :body (left :angle pi|2) (right :angle pi|2) (forward :length 20))
     (branch :from :body (right :angle pi|2) (left :angle pi|2) (forward :length 20)))
    (export "branch.glb" (m/material :color [0. 0.7 0.7 1.0] :metalness 0.2)))

;; Gaps

(-> (extrude
     (frame :cross-section (m/circle 6) :name :body :curve-radius 10)
     (for [i (range 3)]
       [(left :angle (/ Math/PI 2) :gap true)
        (right :angle (/ Math/PI 2))]))
    (export "gaps.glb" (m/material :color [0 0.7 0.7 0] :metalness 0.2)))

;; Segment

(-> (extrude
     (result :name :pipes
             :expr (difference :outer :inner))
     (frame :cross-section (m/circle 6) :name :outer :curve-radius 10)
     (frame :cross-section (m/circle 4) :name :inner)
     (for [i (range 4)]
       (branch
        :from :outer
        (rotate :x (* i 1/2 Math/PI))
        (forward :length 30))))
    (export "pipes.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))

;; Composition

(let [pipe (extrude
            (frame :cross-section (m/circle 6) :name :outer :curve-radius 10)
            (frame :cross-section (m/circle 4) :name :inner)
            (forward :length 30))]
  (-> (extrude
       (result :name :pipes
               :expr (->> (difference :pipe/outer :pipe/inner)
                          (trim-by-plane :normal [-1 0 0])
                          (translate :z 30)))
       (frame :name :origin)

       (for [i (range 4)]
         (branch
          :from :origin
          (rotate :x (* i 1/2 Math/PI))
          (insert :extrusion pipe
                  :models [:outer :inner]
                  :ns :pipe
                  :end-frame :outer))))
      (export "insert.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2))))

;; Points

(-> (m/cross-section
     (points
      :axes [:x :z]
      (frame :name :origin)
      (translate :x 50)
      (left :angle (* 2 Math/PI) :curve-radius 50 :cs 20)))
    (m/extrude 1)
    (export "circle.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))
