(ns examples.pipes
  (:require
   [scad-clj.scad :as s]
   [scad-clj.model :as m]
   [plexus.core
    :refer [result frame left right forward up down hull extrude set branch arc defmodel
            rotate translate segment difference union intersection points]]))

(->> (extrude
      (result :name :pipes
              :expr (difference :body :mask))

      (frame :profile (m/circle 6) :name :body)
      (frame :profile (m/circle 4) :name :mask)
      (set :curve-radius 20 :fn 70 :to [:body]) (set :curve-radius 20 :fn 70 :to [:mask])

      (left :angle (/ Math/PI 2) :to [:body])
      (left :angle (/ Math/PI 2) :to [:mask])

      (right :angle (/ Math/PI 2) :to [:body])
      (right :angle (/ Math/PI 2) :to [:mask])

      (forward :length 10 :to [:body])
      (forward :length 10 :to [:mask])

      (up :to [:body])
      (up :to [:mask]))
     (s/write-scad)
     (spit "test.scad"))

;; Or equivalently:

(->> (extrude
      (result :name :pipes
              :expr (difference :body :mask))

      (frame :profile (m/circle 6) :name :body)
      (frame :profile (m/circle 4) :name :mask)
      (set :curve-radius 20 :fn 70 :to [:body :mask])

      (left :angle (/ Math/PI 2) :to [:body :mask])
      (right :angle (/ Math/PI 2) :to [:body :mask])
      (forward :length 10 :to [:body :mask])
      (up :to [:body :mask])
      (forward :length 20 :to [:body :mask]))
     (s/write-scad)
     (spit "test.scad"))

;; ... Or equivalently:

(->> (extrude
      (result :name :pipes
              :expr (difference :body :mask))

      (frame :profile (m/circle 6) :name :body)
      (frame :profile (m/circle 4) :name :mask)
      (set :curve-radius 20 :fn 70 :to [:body :mask])

      (left :angle (/ Math/PI 2))
      (right :angle (/ Math/PI 2))
      (forward :length 10)
      (up :to [:body :mask])
      (forward :length 20))
     (s/write-scad)
     (spit "test.scad"))

;; Hulls

(->> (extrude
      (result :name :pipes
              :expr (difference :body :mask))

      (frame :profile (m/circle 6) :name :body)
      (frame :profile (m/circle 4) :name :mask)
      (set :curve-radius 20 :fn 70)

      (forward :length 20)

      (set :profile (m/square 20 20) :to [:body])
      (set :profile (m/square 16 16) :to [:mask])

      (forward :length 20)
      (hull)
      (forward :length 20)

      (set :profile (m/circle 6) :to [:body])
      (set :profile (m/circle 4) :to [:mask])

      (forward :length 20)
      (hull))
     (s/write-scad)
     (spit "test.scad"))

;; Branching

(def pi|2 (/ Math/PI 2))

(->> (extrude
      (result :name :pipes
              :expr (difference :body :mask))

      (frame :profile (m/circle 6) :name :body)
      (frame :profile (m/circle 4) :name :mask)
      (set :curve-radius 10 :fn 70)

      (branch :from :body (left :angle pi|2) (right :angle pi|2) (forward :length 20))
      (branch :from :body (right :angle pi|2) (left :angle pi|2) (forward :length 20)))
     (s/write-scad)
     (spit "test.scad"))

;; Gaps

(->> (extrude
      (frame :profile (m/circle 6) :name :body :curve-radius 10 :fn 70)
      (left :angle (/ Math/PI 2) :gap true)
      (right :angle (/ Math/PI 2))
      (left :gap true)
      (right)
      (left :gap true)
      (right))
     (s/write-scad)
     (spit "test.scad"))

;; Arcs


;; Arcs defined by providing the straight-line distance between the start-point
;; and end-point of the arc, along with the radius.
;;
;; In this example, the distance between Z-intersection points will be 20.
;; You can think of it as a two-point circle.

(defmodel arc-torus
  (result :name :arc-torus
          :expr :body)

  (frame :profile (m/circle 2) :fn 50 :name :body)
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

(->> (extrude
      (result :name :pipes
              :expr (difference :outer :inner))
      (frame :profile (m/circle 6) :name :outer :curve-radius 10 :fn 70)
      (frame :profile (m/circle 4) :name :inner)
      (segment
       (for [i (range 4)]
         (branch
          :from :outer
          (rotate :x (* i 1/2 Math/PI))
          (forward :length 30)))))
     (s/write-scad)
     (spit "test.scad"))

(let [pipe-path (extrude
                 (frame :profile (m/circle 6) :name :outer :curve-radius 10 :fn 70)
                 (frame :profile (m/circle 4) :name :inner)
                 (forward :length 30))]
  (->> (extrude
        (result :name :pipes
                :expr (difference :outer :inner))
        (frame :name :origin)

        (segment
         (for [i (range 4)]
           (branch
            :from :origin
            (rotate :x (* i 1/2 Math/PI))
            (segment pipe-path)))))
       (s/write-scad)
       (spit "test.scad")))

;; Points

(->> (m/polygon
      (points
       :axes [:x :z]
       (frame :name :origin :fn 20)
       (translate :x 50)
       (left :angle (* 2 Math/PI) :curve-radius 50)))
     (s/write-scad)
     (spit "test.scad"))
