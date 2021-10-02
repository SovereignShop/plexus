(ns scad-paths.transforms
  (:require
   [clojure.core.matrix :as mat]))

(defrecord Transform [direction pos])

(defn rodrigues-rotation
  [v k a]
  (mat/add
   (mat/mmul (vec v)
             (Math/cos a))
   (mat/mmul (mat/cross k v)
             (Math/sin a))
   (mat/mmul (vec k)
             (mat/dot k v)
             (- 1 (Math/cos a)))))

(defn yaw
  ([{[vx vy vz] :direction :as m} a]
   (let [d [(rodrigues-rotation vx vy a)
            vy
            (rodrigues-rotation vz vy a)]]
     (assoc m :direction d))))

(defn pitch
  ([{[vx vy vz] :direction :as m} a]
   (let [d [vx
            (rodrigues-rotation vy vx a)
            (rodrigues-rotation vz vx a)]]
     (assoc m :direction d))))

(defn roll
  ([{[vx vy vz] :direction :as m} a]
   (let [d [(rodrigues-rotation vx vz a)
            (rodrigues-rotation vy vz a)
            vz]]
     (assoc m :direction d))))

(defn rotate [{[vx vy vz] :direction :as transform} axis a]
  (let [d [(rodrigues-rotation vx axis a)
           (rodrigues-rotation vy axis a)
           (rodrigues-rotation vz axis a)]]
    (assoc transform :direction d)))

(defn go-forward [{:keys [direction pos] :as transform} x]
  (let [v  (mat/select direction 2)
        tr (mat/add pos (mat/mmul v x))]
    (assoc transform :pose tr)))

(defn go-backward [{:keys [direction pos] :as transform} x]
  (let [v  (mat/select direction 2)
        tr (mat/sub pos (mat/mmul v x))]
    (assoc transform :pose tr)))
