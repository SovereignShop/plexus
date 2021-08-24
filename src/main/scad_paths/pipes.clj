(ns scad-paths.pipes
  (:require
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defn quarter-pipe
  [or ir & {:keys [shell]}]
  (u/torus or ir :shell shell :angle 90))

(defn pipe
  [or ir length & {:keys [center ] :or {center true}}]
  (->> (u/circle-shell or ir :height length :center center)
       (m/rotatec [(- (u/half u/pi)) 0 0])))

(defn half-pipe
  [or ir & {:keys [shell]}]
  (u/torus or ir :shell shell :angle 180))
