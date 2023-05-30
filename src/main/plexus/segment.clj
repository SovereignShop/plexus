(ns plexus.segment
  (:import [manifold3d.glm MatrixTransforms])
  (:require
   [scad-clj.model :as m]
   [clojure.core.matrix :as mat]
   [plexus.utils :as u]
   [plexus.transforms :as tf]))

#_(defn get-rotation
  "Get rotation transform to start of segment."
  [segment]
  (-> segment meta :start-transform tf/rotation-matrix))

(defn get-transform
  "Get start transformation matrix."
  [segment]
  (:start-transform segment))

(defn set-transform
  "Set segment start rotation."
  [m segment]
  (assoc segment :start-transform m))

(defn get-translation
  "Get transform to end of segment."
  [segment]
  (-> segment :start-transform tf/translation-vector))

(defn set-translation [segment translation]
  (update segment :end-transform tf/set-translation translation))

(defn roll [segment angle]
  (set-transform (tf/roll (get-transform segment) angle) segment))

(defn unplace-segment
  "Send segment from its transform frame to global origin."
  [segment]
  (if (not (:is-placed segment))
    segment
    (let [manifold (:manifold segment)
          m (get-transform segment)]
      (-> segment
          (assoc :manifold (.transform manifold (MatrixTransforms/InvertTransform m)))
          (assoc :is-placed false)))))

(defn place-segment
  "Send segment from global origin to its transform frame."
  [segment]
  (if (:is-placed segment)
    segment
    (let [manifold (:manifold segment)
          m (get-transform segment)]
      (-> segment
          (assoc :manifold (.transform manifold m))
          (assoc :is-placed true)))))
