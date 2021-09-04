(ns scad-paths.core
  (:require
   [clj-tf.utils :as tf]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defn rotation-vector [v k a]
  (tf/add (tf/mul v
                  (Math/cos a))
          (tf/mul (tf/cross k v)
                  (Math/sin a))
          (tf/mul k
                  (tf/dot k v)
                  (- 1 (Math/cos a)))))

(defn turn-left
  ([v] (turn-left v (/ Math/PI 2)))
  ([v a]
   (rotation-vector v [0 0 1] a)))

(defn turn-right
  ([v] (turn-right v (/ Math/PI 2)))
  ([v a]
   (rotation-vector v [0 0 1] (- a))))

(defn turn-up
  ([v] (turn-up v (/ Math/PI 2)))
  ([v a]
   (rotation-vector v [1 0 0] a)))

(defn turn-down
  ([v] (turn-down v (/ Math/PI 2)))
  ([v a]
   (rotation-vector v [1 0 0] (- a))))

(defn go-forward [p v x]
  (tf/add p (tf/mul v x)))

(defmulti path-segment (fn [ctx _ _ _] (:op ctx)))

(defn path [ctx & path]
  (let [curr-context (into {:or 10 :curve-radius 7 :shell 1/2 :fn 100 :shape (u/circle-shell 3 2)}
                           ctx)]
    (loop [pose [0 0 0]
           [[op & args] & steps] path
           ctx curr-context
           ret []]
      (if (nil? op)
        (m/union ret)
        (if (= op ::context)
          (recur pose steps (into ctx (partition-all 2) (vec args)) ret)
          (let [[new-pose parts] (path-segment (assoc ctx :op op) pose ret args)]
            (recur new-pose steps ctx parts)))))))

(defn path-grid
  ([ctx path]
   (path-grid ctx ctx path))
  ([outer-context inner-context path]
   (let [default-context {:or 10 :curve-radius 7 :shell 1/2 :fn 100 :shape (u/circle-shell 3 2) :pose [0 0 0]}
         merge-fn (partial merge-with into)]
     (loop [ret {:outer-context (into default-context outer-context)
                 :inner-context (into default-context inner-context)
                 :outer-form []
                 :inner-form []}
            [seg & segments] path]
       (let [[l r] (if (vector? seg) seg [seg])
             outer-context (:outer-context ret)
             outer-pose (:pose outer-context)
             inner-context (:inner-context ret)
             inner-pose (:pose inner-context)]
         (cond (nil? l)
               ret

               (= l :branch)
               (let [ret (merge-fn ret
                                   (-> (path-grid outer-context inner-context r)
                                       (dissoc :outer-context :inner-context)))]
                 (recur ret segments))

               (= l :model)
               (let [k (first r)
                     p (next r)
                     r (path-grid outer-context inner-context p)]
                 (recur (-> (merge-fn ret r)
                            (assoc-in [:models k] r))
                        segments))

               (= l :extra-model)
               (let [k (first r)
                     p (next r)
                     r (path-grid outer-context inner-context p)]
                 (recur (-> ret
                            (assoc-in [:models k] r))
                        segments))

               (= (first l) ::context)
               (recur (-> ret
                          (update :outer-context into (partition-all 2) (next l))
                          (update :inner-context into (partition-all 2) (next r)))
                      segments)

               :else
               (let [[l-op & l-args :as left] l
                     [r-op & r-args] (if (nil? r) left r)
                     outer-form (:outer-form ret)
                     inner-form (:inner-form ret)
                     [outer-pose outer] (path-segment (assoc outer-context :op l-op) outer-pose outer-form l-args)
                     [inner-pose inner] (path-segment (assoc inner-context :op r-op) inner-pose inner-form r-args)]
                 (recur (-> ret
                            (assoc :outer-form outer)
                            (assoc :inner-form inner)
                            (update :outer-context assoc :pose outer-pose)
                            (update :inner-context assoc :pose inner-pose))
                        segments))))))))

(defmethod path-segment ::left
  [{:keys [fn shape gap] :as ctx} [x y angle] segments args]
  (let [[& {:keys [curve-radius]
            :or {curve-radius (:curve-radius ctx)}}] args
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (- angle (/ u/pi 2))]
    [[(+ x (* curve-radius (- (- (Math/cos angle) (Math/cos new-angle)))))
      (+ y (* curve-radius (- (- (Math/sin new-angle) (Math/sin angle)))))
      new-angle]
     (if gap
       segments
       (conj segments part))]))

(defmethod path-segment ::right
  [{:keys [fn shape gap] :as ctx} [x y angle] segments args]
  (let [[& {:keys [curve-radius]
            :or {curve-radius (:curve-radius ctx)}}] args
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])
                    (m/rotatec [0 0 (- angle)])
                    (m/translate [x y 0])))
        new-angle (+ angle (/ u/pi 2))]
    [[(+ x (* curve-radius (- (Math/cos angle) (Math/cos new-angle))))
      (+ y (* curve-radius (- (Math/sin new-angle) (Math/sin angle))))
      new-angle]
     (if gap
       segments
       (conj segments part))]))

(defmethod path-segment ::curve
  [{:keys [fn shape gap] :as ctx} [x y angle] segments args]
  (let [[& {:keys [curve-radius curve-angle]
            :or {curve-radius (:curve-radius ctx)}}] args
        part (binding [m/*fn* fn]
               (if (pos? curve-angle)
                 (->> shape
                      (m/translate [curve-radius 0 0])
                      (m/extrude-rotate {:angle curve-angle})
                      (m/translate [(- curve-radius) 0 0])
                      (m/rotatec [0 u/pi 0])
                      (m/rotatec [0 0 (- angle)])
                      (m/translate [x y 0]))
                 (->> shape
                      (m/translate [curve-radius 0 0])
                      (m/extrude-rotate {:angle (Math/abs curve-angle)})
                      (m/translate [(- curve-radius) 0 0])
                      (m/rotatec [0 0 (- angle)])
                      (m/translate [x y 0]))))
        new-angle (+ angle (* 0.01745329 curve-angle))]
    [[(+ x (* curve-radius ((if (pos? curve-angle) + -)
                            (- (Math/cos angle) (Math/cos new-angle)))))
      (+ y (* curve-radius ((if (pos? curve-angle) + -)
                            (- (Math/sin new-angle) (Math/sin angle)))))
      new-angle]
     (if gap
       segments
       (conj segments part))]))

(defmethod path-segment ::forward
  [{:keys [fn shape]} [x y angle] segments [& {:keys [length model twist gap mask]}]]
  (let [part (binding [m/*fn* fn]
               (as-> (if model
                       model
                       (->> shape
                            (m/extrude-linear {:height length :center false :twist twist}))) m
                 (m/rotatec [(- (u/half u/pi)) 0 0] m)
                 (if mask
                   (m/difference m mask)
                   m)
                 (m/rotatec [0 0 (- angle)] m)
                 (m/translate [x y 0] m)))]
    [[(+ x (* length (Math/sin angle)))
      (+ y (* length (Math/cos angle)))
      angle]
     (if gap
       segments
       (conj segments part))]))

(defmethod path-segment ::hull
  [{:keys [fn]} pose segments _]
 [(p/context) (p/context :shape check-valve-inner-circle)] (let [part (binding [m/*fn* fn]
               (m/hull (-> segments pop peek)
                       (peek segments)))]
    [pose
     (conj (-> segments pop pop) part)]))

(defmethod path-segment ::branch
  [ctx [x y angle :as pose] segments args]
  (let [model (apply path ctx args)]
    [pose
     (conj segments
           (->> model
                (m/rotatec [0 0 (- angle)])
                (m/translate [x y 0])))]))

(defn left [& opts]
  `(::left ~@opts))

(defn right [& opts]
  `(::right ~@opts))

(defn curve [& opts]
  `(::curve ~@opts))

(defn forward [& opts]
  `(::forward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))

(defn branch [& args]
  `(::branch ~@args))

(defn context [& args]
  `(::context ~@args))

(defn model [& args]
  `(::model ~@args))
