(ns scad-paths.core
  (:require
   [clj-tf.utils :as tf]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(ns-unmap 'scad-paths.core 'path-segment)

(defmulti path-segment (fn [op translation rotation segments args] op))

(defn path [args & path]
  (let [curr-args (into {:or 10 :curve-radius 7 :shell 1/2 :fn 100 :shape (u/circle-shell 3 2)}
                        args)]
    (loop [translation [0 0 0]
           rotation (tf/matrix33)
           [[op & opts] & steps] path
           args curr-args
           ret []]
      (if (nil? op)
        (m/union ret)
        (let [options (into args (apply hash-map opts))
              [new-tr new-rot parts] (path-segment op translation rotation ret options)]
          (recur new-tr new-rot steps options parts))))))

(defmethod path-segment ::left
  [_ [x y _] rotation segments {:keys [curve-radius fn shape]}]
  (let [angle (tf/yaw rotation)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate  [x y 0])))
        new-angle (- angle (/ u/pi 2))]
    [[(+ x
         (* curve-radius (Math/sin angle))
         (* curve-radius (Math/sin new-angle)))
      (+ y
         (* curve-radius (Math/cos angle))
         (* curve-radius (Math/cos new-angle)))
      0]
     (tf/mul rotation (tf/euler->rotation-matrix [(- (/ u/pi 4)) 0 0]))
     (conj segments part)]))

(tf/yaw
 (tf/mul (tf/euler->rotation-matrix [(- (/ u/pi 4)) 0 0])
         (tf/euler->rotation-matrix [(- (/ u/pi 4)) 0 0])))

(defmethod path-segment ::right
  [_ [x y angle] segments {:keys [curve-radius fn shape]}]
  (let [part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (+ angle (/ u/pi 2))]
    [[(+ x
         (* curve-radius (Math/sin angle))
         (* curve-radius (Math/sin new-angle)))
      (+ y
         (* curve-radius (Math/cos angle))
         (* curve-radius (Math/cos new-angle)))
      new-angle]
     (conj segments part)]))

(defmethod path-segment ::curve
  [_ [x y angle] segments {:keys [curve-radius curve-angle fn shape]}]
  (let [part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle curve-angle})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (+ angle (* curve-angle 0.01745329))]
    [[(+ x
         (* curve-radius (Math/sin angle))
         (* curve-radius (Math/sin new-angle)))
      (+ y
         (* curve-radius (Math/cos angle))
         (* curve-radius (Math/cos new-angle)))
      new-angle]
     (conj segments part)]))

(defmethod path-segment ::forward
  [_ [x y angle] segments {:keys [fn length shape] :or {length 10}}]
  (let [part (binding [m/*fn* fn]
               (->> shape
                    (m/extrude-linear {:height length :center false})
                    (m/rotatec [(- (u/half u/pi)) 0 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))]
    [[(+ x (* length (Math/sin angle)))
      (+ y (* length (Math/cos angle)))
      angle]
     (conj segments part)]))

(defmethod path-segment ::hull
  [_ pose segments {:keys [fn]}]
  (let [part (binding [m/*fn* fn]
               (m/hull (-> segments pop peek)
                       (peek segments)))]
    [pose (conj (-> segments pop pop) part)]))

(defmethod path-segment ::branch
  [_ pose segments {:keys [branches] :as options}]
  (let [opts (dissoc options :branches)]
    [pose (conj segments (m/union (map #(path opts branches) branches)))]))

(defn left [& opts]
  `(::left ~@opts))

(defn right [& opts]
  `(::right ~@opts))

(defn curve [& opts]
  `(::curve ~@opts))

(defn branch [& opts]
  `(::branch ~@opts))

(defn forward [& opts]
  `(::forward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))
