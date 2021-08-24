(ns scad-paths.core
  (:require
   [scad-paths.utils :as u]
   [scad-paths.pipes :as pipes]
   [scad-clj.model :as m]))

(defmulti path-segment (fn [op pose args] op))

(defn path [& path]
  (let [curr-args {:or 10 :ir 7 :shell 1 :fn 100}]
    (loop [pose [0 0 0]
           [[op & opts] & steps] path
           args curr-args
           ret []]
      (if (nil? op)
        (m/union ret)
        (let [options (into args (apply hash-map opts))
              [new-pose part] (path-segment op pose options)]
          (recur new-pose steps options (conj ret part)))))))

(defmethod path-segment :pipe/left
  [_ [x y angle] {:keys [or ir shell fn]}]
  (let [tr (+ ir (u/half (- or ir)))
        part (binding [m/*fn* fn]
               (->> (pipes/quarter-pipe or ir :shell shell)
                    (m/translate [(- tr) 0 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (- angle (/ u/pi 2))]
    [[(+ x
         (* tr (Math/sin angle))
         (* tr (Math/sin new-angle)))
      (+ y
         (* tr (Math/cos angle))
         (* tr (Math/cos new-angle)))
      new-angle]
     part]))


(defmethod path-segment :pipe/right
  [_ [x y angle] {:keys [or ir shell fn]}]
  (let [tr (+ ir (u/half (- or ir)))
        part (binding [m/*fn* fn]
               (->> (pipes/quarter-pipe or ir :shell shell)
                    (m/translate [(- tr) 0 0])
                    (m/rotatec [0 u/pi 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (+ angle (/ u/pi 2))]
    [[(+ x
         (* tr (Math/sin angle))
         (* tr (Math/sin new-angle)))
      (+ y
         (* tr (Math/cos angle))
         (* tr (Math/cos new-angle)))
      new-angle]
     part]))

(defmethod path-segment :pipe/straight
  [_ [x y angle] {:keys [or ir shell fn length] :or {length 10}}]
  (let [thickness (- or ir)
        part (binding [m/*fn* fn]
               (->> (pipes/pipe thickness (- thickness (* 2 shell)) length :center false)
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))]
    [[(+ x (* length (Math/sin angle)))
      (+ y (* length (Math/cos angle)))
      angle]
     part]))
