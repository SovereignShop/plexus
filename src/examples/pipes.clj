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
    (export "test.glb"))

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
    (export "pipes.glb"))

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
    (export "pipes.glb"))

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
    (export "hull.glb"))

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
    (export "loft.glb"))

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
    (export "branch.glb"))

;; Gaps

(-> (extrude
     (frame :cross-section (m/circle 6) :name :body :curve-radius 10)
     (for [i (range 3)]
       [(left :angle (/ Math/PI 2) :gap true)
        (right :angle (/ Math/PI 2))]))
    (export "gaps.glb"))

;; Arcs


;; Arcs defined by providing the straight-line distance between the start-point
;; and end-point of the arc, along with the radius.
;;
;; In this example, the distance between Z-intersection points will be 20.
;; You can think of it as a two-point circle.

(defmodel arc-torus
  (result :name :arc-torus
          :expr :body)

  (frame :cross-section (m/circle 2) :fn 50 :name :body)
  (arc :side-length 20 :curve-radius 5)
  #_(arc :side-length 20 :curve-radius 20)
  #_(arc :side-length 20 :curve-radius 20)
  #_(arc :side-length 20 :curve-radius 20)
  #_(arc :side-length 20 :curve-radius 20)
  #_(arc :side-length 20 :curve-radius 20))

(->> arc-torus
     (s/write-scad)
     (spit "test.scad"))

;; Segment

(-> (extrude
     (result :name :pipes
             :expr (difference :outer :inner))
     (frame :cross-section (m/circle 6) :name :outer :curve-radius 10 :fn 70)
     (frame :cross-section (m/circle 4) :name :inner)
     (for [i (range 4)]
       (branch
        :from :outer
        (rotate :x (* i 1/2 Math/PI))
        (forward :length 30))))
    (export "pipes.glb"))

;; Composition

(let [pipe-path (extrude
                 (frame :cross-section (m/circle 6) :name :outer :curve-radius 10)
                 (frame :cross-section (m/circle 4) :name :inner)
                 (forward :length 30))]
  (-> (extrude
       (result :name :pipes
               :expr (trim-by-plane {:normal [-1 0 0]} (difference :pipe/outer :pipe/inner)))
       (frame :name :origin)
       (translate :z 5)

       (for [i (range 4)]
         (branch
          :from :origin
          (rotate :x (* i 1/2 Math/PI))
          (insert :extrusion pipe-path
                  :models [:outer :inner]
                  :ns :pipe
                  :end-frame :outer))))
      (export "insert.glb")))

;; Points

(-> (m/cross-section
     (points
      :axes [:x :z]
      (frame :name :origin)
      (translate :x 50)
      (left :angle (* 2 Math/PI) :curve-radius 50 :cs 20)))
    (m/extrude 1)
    (export "circle.glb"))
