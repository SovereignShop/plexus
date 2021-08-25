(ns scad-paths.core
  (:require
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-segment (fn [op pose segments args] op))

(defn path [args & path]
  (let [curr-args (into {:or 10 :curve-radius 7 :shell 1/2 :fn 100 :shape (u/circle-shell 3 2)}
                        args)]
    (loop [pose [0 0 0]
           [[op & opts] & steps] path
           args curr-args
           ret []]
      (if (nil? op)
        (m/union ret)
        (let [options (into args (apply hash-map opts))
              [new-pose parts] (path-segment op pose ret options)]
          (recur new-pose steps options parts))))))

(defmethod path-segment ::left
  [_ [x y angle] segments {:keys [curve-radius fn shape]}]
  (let [part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (- angle (/ u/pi 2))]
    [[(+ x
         (* curve-radius (Math/sin angle))
         (* curve-radius (Math/sin new-angle)))
      (+ y
         (* curve-radius (Math/cos angle))
         (* curve-radius (Math/cos new-angle)))
      new-angle]
     (conj segments part)]))

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
  [_ [x y angle] segments {:keys [fn]}]
  (let [part (binding [m/*fn* fn]
               (m/hull (-> segments pop peek)
                       (peek segments)))]
    [[x y angle]
     (conj (-> segments pop pop) part)]))

(defn left [& opts]
  `(::left ~@opts))

(defn right [& opts]
  `(::right ~@opts))

(defn forward [& opts]
  `(::forward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))
