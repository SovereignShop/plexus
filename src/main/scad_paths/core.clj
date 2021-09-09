(ns scad-paths.core
  (:require
   [clj-tf.utils :as tf]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defn rotation-vector
  "Rodrigues rotation formula"
  [v k a]
  (tf/add (tf/mul (vec v)
                  (Math/cos a))
          (tf/mul (tf/cross k v)
                  (Math/sin a))
          (tf/mul (vec k)
                  (tf/dot k v)
                  (- 1 (Math/cos a)))))

(defn turn-left
  ([v]
   (turn-left v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [(rotation-vector vx vz a)
    (rotation-vector vy vz a)
    vz]))

(defn turn-right
  ([v]
   (turn-right v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [(rotation-vector vx vz (- a))
    (rotation-vector vy vz (- a))
    vz]))

(defn turn-up
  ([v]
   (turn-up v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [vx
    (rotation-vector vy vx a)
    (rotation-vector vz vx a)]))

(defn turn-down
  ([v]
   (turn-down v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [vx
    (rotation-vector vy vx (- a))
    (rotation-vector vz vx (- a))]))

(defn roll-left
  ([v]
   (roll-left v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [(rotation-vector vx vy a)
    vy
    (rotation-vector vz vy a)]))

(defn roll-right
  ([v]
   (roll-left v (/ Math/PI 2)))
  ([[vx vy vz] a]
   [(rotation-vector vx vy (- a))
    vy
    (rotation-vector vz vy (- a))]))

(defn rotate
  [[vx vy vz] axis a]
  [(rotation-vector vx axis a)
   (rotation-vector vy axis a)
   (rotation-vector vz axis a)])

(defn angle-between [a b]
  (Math/acos (/ (tf/dot a b) (* (tf/mag a) (tf/mag b)))))

(defn go-forward [p v x]
  (tf/add p (tf/mul (second v) x)))

(defn about-equal? [v1 v2]
  (loop [[x & xs] v1
         [y & ys] v2]
    (cond
      (nil? x) true

      (< (Math/abs (- x y)) 0.00001)
      (recur xs ys)

      :else false)))

(defn opposing? [v1 v2]
  (about-equal? (tf/add v1 v2) [0 0 0]))

(defn ortho-angle [v1 v2 opp]
  (let [cross (tf/cross v1 v2)]
    (if (about-equal? cross [0 0 0])
      (if (opposing? v1 v2)
        [Math/PI opp]
        [0 opp])
      [(angle-between v1 v2) (tf/normalise cross)])))

(defn about= [a b]
  (< (Math/abs (- a b))
     0.00001))

(defn scad-transform
  ([m pose] (scad-transform [[1 0 0] [0 1 0] [0 0 1]] m pose))
  ([a b pose]
   (let [[angle ortho] (ortho-angle (nth a 0) (nth b 0) [0 0 1])
         c (rotate a ortho angle)
         [angle-2 ortho-2] (ortho-angle (nth b 1) (nth c 1) (nth c 0))]
     (comp
      (partial m/translate pose)
      (partial m/rotatev (- angle-2) ortho-2)
      (partial m/rotatev angle ortho)))))

(defmulti path-segment (fn [ctx _] (:op ctx)))

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
   (let [default-context {:or 10
                          :curve-radius 7
                          :shell 1/2
                          :fn 10
                          :shape (m/circle 5)
                          :form []
                          :pose [0 0 0]
                          :axes [[1 0 0]
                                 [0 1 0]
                                 [0 0 1]]}
         merge-fn (partial merge-with into)]
     (loop [ret {:outer-context (into default-context outer-context)
                 :inner-context (into default-context inner-context)}
            [seg & segments] path]
       (let [[l r] (if (vector? seg) seg [seg])
             outer-context (:outer-context ret)
             inner-context (:inner-context ret)]
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
                     new-outer-ctx (path-segment (assoc outer-context :op l-op) l-args)
                     new-inner-ctx (path-segment (assoc inner-context :op r-op) r-args)]
                 (recur (assoc ret :outer-context new-outer-ctx :inner-context new-inner-ctx)
                        segments))))))))

(defn bAc->a
  "side,angle,side -> side via. law of cosines."
  [b A c]
  (Math/sqrt (- (+ (Math/pow b 2)
                   (Math/pow c 2))
                (* 2 b c (Math/cos A)))))

(defmethod path-segment ::left
  [{:keys [fn shape gap pose axes rot] :as ctx} args]
  (let [[& {:keys [curve-radius]
            :or {curve-radius (:curve-radius ctx)}}] args
        transform (scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    transform))
        new-pose (go-forward pose axes curve-radius)
        new-axes (turn-left axes)
        new-pose (go-forward new-pose new-axes curve-radius)]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::right
  [{:keys [fn shape gap pose axes rot] :as ctx} args]
  (let [[& {:keys [curve-radius]
            :or {curve-radius (:curve-radius ctx)}}] args
        rt       (comp
                  (partial m/translate [curve-radius curve-radius 0])
                  (partial m/rotatec [0 0 (- (/ Math/PI 2))])
                  rot)
        transform (scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])
                    transform))
        new-pose (go-forward pose axes curve-radius)
        new-axes (turn-right axes)
        new-pose (go-forward new-pose new-axes curve-radius)]
    (cond-> (assoc ctx :pose new-pose :axes new-axes :rot rt)
      (not gap) (update :form conj part))))

(defmethod path-segment ::up
  [{:keys [fn shape gap pose axes rot] :as ctx} args]
  (let [[& {:keys [curve-radius angle]
            :or {curve-radius (:curve-radius ctx)}}] args
        transform (scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (/ u/pi 2) 0])
                    transform))
        new-pose (go-forward pose axes curve-radius)
        new-axes (turn-up axes (or angle (/ Math/PI 2)))
        new-pose (go-forward new-pose new-axes curve-radius)]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))


(defmethod path-segment ::down
  [{:keys [fn shape gap pose axes rot] :as ctx} args]
  (let [[& {:keys [curve-radius angle]
            :or {curve-radius (:curve-radius ctx)}}] args
        transform (scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle 90})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (/ u/pi 2) 0])
                    transform))
        new-pose (go-forward pose axes curve-radius)
        new-axes (turn-down axes (or angle (/ Math/PI 2)))
        new-pose (go-forward new-pose new-axes curve-radius)]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::forward
  [{:keys [fn shape pose axes] :as ctx} [& {:keys [length model twist gap mask]}]]
  (let [transform (scad-transform axes pose)
        part (binding [m/*fn* fn]
               (as-> (if model
                       model
                       (->> shape
                            (m/extrude-linear {:height length :center false :twist twist}))) m
                 (m/rotatec [(- (u/half u/pi)) 0 0] m)
                 (if mask
                   (m/difference m mask)
                   m)
                 (transform m)))
        new-pose (go-forward pose axes length)]
    (cond-> (assoc ctx :pose new-pose)
      (not gap) (update :form conj part))))

#_(defmethod path-segment ::hull
  [{:keys [fn]} pose segments _]
  (let [part (binding [m/*fn* fn]
               (m/hull (-> segments pop peek)
                       (peek segments)))]
    [pose
     (conj (-> segments pop pop) part)]))

#_(defmethod path-segment ::branch
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

(defn up [& opts]
  `(::up ~@opts))

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


(->
 (path-grid {}
            [(left) (up) (right) (forward :length 10) (right) (right) (left) (forward :length 30) (right) (up)
             (forward :length 100)])
 :outer-context :form m/union)
