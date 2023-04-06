(ns plexus.segment
  (:require
   [scad-clj.model :as m]
   [clojure.core.matrix :as mat]
   [plexus.utils :as u]))

(defn get-rotation
  "Get rotation transform to start of segment."
  [segment]
  (-> segment meta :start-transform u/rotation-matrix))

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
  (-> segment meta :start-transform u/translation-vector))

(defn set-translation [segment translation]
  (vary-meta segment update :end-transform u/translate translation))

(defn- set-meta [meta segment]
  (with-meta segment meta))

(defn roll [segment angle]
  (set-transform (u/roll (get-transform segment) angle) segment))

(defn get-sub-segment [segment n]
  (-> segment meta :segments (nth n)))

(defn normalise
  "Send segment from global origin to its transform frame."
  [segment]
  (let [met (meta segment)]
    (if (:projected met)
      segment
      (let [m (get-transform segment)
            translation (u/translation-vector m)
            a (get-rotation segment)
            b (u/rotation-matrix u/identity-mat)
            [angle ortho] (u/rotation-axis-and-angle (nth a 0) (nth b 0) [0 0 1])
            c (u/rotate33 a ortho angle)
            [angle-2 ortho-2] (u/rotation-axis-and-angle (nth b 1) (nth c 1) (nth c 0))]
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
            translation (u/translation-vector m)
            a (u/rotation-matrix u/identity-mat)
            b (get-rotation segment)
            [angle ortho] (u/rotation-axis-and-angle (nth a 0) (nth b 0) [0 0 1])
            c (u/rotate33 a ortho angle)
            [angle-2 ortho-2] (u/rotation-axis-and-angle (nth b 1) (nth c 1) (nth c 0))]
        (with-meta
          (->> segment
               (m/rotatev angle ortho)
               (m/rotatev (- angle-2) ortho-2)
               (m/translate translation))
          (assoc met :projected true))))))
