(ns scad-paths.core
  (:require
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-segment (fn [ctx _] (:op ctx)))

(defn path
  ([ctx path-spec]
   (path ctx ctx path-spec))
  ([outer-context inner-context path-spec]
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
            [seg & segments] path-spec]
       (let [[l r] (if (vector? seg) seg [seg])
             outer-context (:outer-context ret)
             inner-context (:inner-context ret)]
         (cond (nil? l)
               ret

               (= l :branch)
               (let [ret (merge-fn ret
                                   (-> (path outer-context inner-context r)
                                       (dissoc :outer-context :inner-context)))]
                 (recur ret segments))

               (= l :model)
               (let [k (first r)
                     p (next r)
                     r (path outer-context inner-context p)]
                 (recur (-> (merge-fn ret r)
                            (assoc-in [:models k] r))
                        segments))

               (= l :extra-model)
               (let [k (first r)
                     p (next r)
                     r (path outer-context inner-context p)]
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

(defmethod path-segment ::left
  [{:keys [fn shape gap pose axes] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees (* angle 57.29578)
        transform (u/->scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    transform))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        new-axes (u/yaw axes r)
        new-pose (u/go-forward pose new-axes d)
        new-axes (u/yaw new-axes (- angle r))]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::right
  [{:keys [fn shape gap pose axes] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees (* angle 57.29578)
        transform (u/->scad-transform axes pose)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])
                    transform))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        new-axes (u/yaw axes (- r))
        new-pose (u/go-forward pose new-axes d)
        new-axes (u/yaw new-axes (- (- angle r)))]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::up
  [{:keys [fn shape gap pose axes] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        transform (u/->scad-transform axes pose)
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (/ u/pi 2) 0])
                    transform))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        new-axes (u/pitch axes r)
        new-pose (u/go-forward pose new-axes d)
        new-axes (u/pitch new-axes (- angle r))]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::down
  [{:keys [fn shape gap pose axes rot] :as ctx} args]
  (let [[& {:keys [curve-radius angle]
            :or {curve-radius (:curve-radius ctx)}}] args
        transform (u/->scad-transform axes pose)
        degrees  (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (/ u/pi 2) 0])
                    transform))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        new-axes (u/pitch axes (- r))
        new-pose (u/go-forward pose new-axes d)
        new-axes (u/pitch new-axes (- (- angle r)))]
    (cond-> (assoc ctx :pose new-pose :axes new-axes)
      (not gap) (update :form conj part))))

(defmethod path-segment ::forward
  [{:keys [fn shape pose axes gap] :as ctx} args]
  (let [[& {:keys [length model twist gap mask]
            :or {gap gap}}]  args
        transform (u/->scad-transform axes pose)
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
        new-pose (u/go-forward pose axes length)]
    (cond-> (assoc ctx :pose new-pose)
      (not gap) (update :form conj part))))

(defmethod path-segment ::roll
  [{:keys [axes] :as ctx} [& {:keys [angle] :or {angle (/ Math/PI 2)}}]]
  (let [new-axes (u/roll axes angle)]
    (assoc ctx :axes new-axes)))

(defmethod path-segment ::hull
  [{:keys [fn form] :as ctx} [& {:keys [n-segments] :or {n-segments 2}}]]
  (let [hull-forms (into () (take n-segments) (map peek (iterate pop form)))
        other-forms (nth (iterate pop form) n-segments)
        part (binding [m/*fn* fn]
               (m/hull hull-forms))]
    (assoc ctx :form (conj other-forms part))))

(defn left [& opts]
  `(::left ~@opts))

(defn right [& opts]
  `(::right ~@opts))

(defn up [& opts]
  `(::up ~@opts))

(defn down [& opts]
  `(::down ~@opts))

(defn roll [& opts]
  `(::roll ~@opts))

(defn forward [& opts]
  `(::forward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))

(defn context [& args]
  `(::context ~@args))

(defn model [& args]
  `(::model ~@args))

(comment

  (->
   (path {:curve-radius 20}
         [(left :angle (/ Math/PI 1.5)) (up) (right :angle (/ Math/PI 6)) (roll) (forward :length 10) (right) (right) (hull :n-segments 4) (left) (forward :length 30) (right) (up) (forward :length 100)])
   :outer-context :form m/union)

  )
