(ns plexus.segment
  (:require
   [scad-clj.model :as m]
   [clojure.core.matrix :as mat]
   [plexus.utils :as u]
   [plexus.transforms :as tf]))

(defn get-rotation
  "Get rotation transform to start of segment."
  [segment]
  (-> segment meta :start-transform tf/rotation-matrix))

(defn get-transform
  "Get start transformation matrix."
  [segment]
  (-> segment meta :start-transform))

(defn set-transform
  "Set segment start rotation."
  [m segment]
  (vary-meta segment assoc :start-transform m))

(defn set-transform
  "Set segment start rotation."
  [m segment]
  (vary-meta segment assoc :start-transform m))

(defn get-translation
  "Get transform to end of segment."
  [segment]
  (-> segment meta :start-transform tf/translation-vector))

(defn set-translation [segment translation]
  (vary-meta segment update :end-transform tf/set-translation translation))

(defn- set-meta [meta segment]
  (with-meta segment meta))

(defn roll [segment angle]
  (set-transform (tf/roll (get-transform segment) angle) segment))

(defn get-sub-segment [segment n]
  (-> segment meta :segments (nth n)))

(defn normalise
  "Send segment from global origin to its transform frame."
  [segment]
  (let [met (meta segment)]
    (if (:projected met)
      segment
      (let [a (get-transform segment)
            translation (tf/translation-vector a)
            b tf/identity-tf
            [angle ortho] (tf/rotation-axis-and-angle (tf/rx a) (tf/rx b) [0 0 1])
            c (tf/rotate a ortho angle)
            [angle-2 ortho-2] (tf/rotation-axis-and-angle (tf/ry b) (tf/ry c) (tf/rx c))]
        (with-meta
          (->> segment
               (m/translate (mat/mul -1 translation))
               (m/rotatev angle ortho)
               (m/rotatev (- angle-2) ortho-2))
          (dissoc met :projected))))))

(defn project
  "Send segment from global origin to its transform frame."
  [segment]
  (let [met (meta segment)]
    (if (:projected met)
      segment
      (let [m (get-transform segment)
            translation (tf/translation-vector m)
            a tf/identity-tf
            b (get-transform segment)
            [angle ortho] (tf/rotation-axis-and-angle (tf/rx a) (tf/rx b) [0 0 1])
            c (tf/rotate a ortho angle)
            [angle-2 ortho-2] (tf/rotation-axis-and-angle (tf/ry b) (tf/ry c) (tf/rx c))]
        (with-meta
          (->> segment
               (m/rotatev angle ortho)
               (m/rotatev (- angle-2) ortho-2)
               (m/translate translation))
          (assoc met :projected true))))))
