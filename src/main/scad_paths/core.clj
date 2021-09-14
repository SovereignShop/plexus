(ns scad-paths.core
  (:require
   [scad-clj.scad :as s]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-segment (fn [_ ctx _] (:op ctx)))

(defn column->model
  ([column] (column->model column m/union true))
  ([column op branch?]
   (let [start-tf (:start-transform (first column))
         inverse-tf (u/->inverse-scad-transform start-tf u/identity-mat)]
     (loop [end-tf start-tf
            [{:keys [start-transform end-transform model branch]
              :as seg} & column] column
            ret []]
       (if (nil? seg)
         (with-meta
           (if (not branch?) (inverse-tf (op ret)) (op ret))
           {:start-transform start-tf
            :end-transform end-tf})
         (let [f (u/->scad-transform u/identity-mat start-transform)]
           (recur end-transform
                  column
                  (cond-> ret
                    model (conj (f model))
                    (and branch branch?) (conj (column->model branch op branch?))))))))))

(defn to-models [segments]
  (for [column segments]
    (column->model column)))

(defn ->model [segments path-spec]
  (let [[outer-model inner-model] (to-models segments)]
    (with-meta
      (m/difference outer-model inner-model)
      {:segments segments
       :path-spec path-spec
       :start-transform (-> outer-model meta :start-transform)
       :end-transform (-> outer-model meta :end-transform)
       :outer-model outer-model
       :inner-model inner-model})))

(defn update-last [v f & args]
  (conj (pop v) (apply f (peek v) args)))

(defn path
  ([path-spec]
   (path nil path-spec))
  ([ctx path-spec]
   (path ctx ctx path-spec))
  ([outer-context inner-context path-spec]
   (let [default-context {:or 10
                          :curve-radius 7
                          :shell 1/2
                          :fn 10
                          :shape (binding [m/*fn* (:fn outer-context 10)]
                                   (m/circle 5))
                          :form []
                          :start-transform [[1 0 0 0]
                                            [0 1 0 0]
                                            [0 0 1 0]
                                            [0 0 0 0]]
                          :end-transform [[1 0 0 0]
                                          [0 1 0 0]
                                          [0 0 1 0]
                                          [0 0 0 0]]}]
     (loop [ret [[(into default-context outer-context)]
                 [(into default-context inner-context)]]
            [seg & segments] path-spec]
       (let [[l r] (if (vector? seg) seg [seg])
             outer (nth ret 0)
             outer-context (peek outer)
             inner (nth ret 1)
             inner-context (peek inner)]
         (cond (nil? l)
               (->model ret path-spec)

               (= l :segment)
               (recur ret (into (or (-> r meta :path-spec) r) segments))

               (= l :branch)
               (let [model (path outer-context inner-context (:path-spec (meta r) r))
                     [branch-outer branch-inner] (-> model meta :segments)]
                 (recur [(conj (pop outer) (assoc outer-context :branch branch-outer))
                         (conj (pop inner) (assoc inner-context :branch branch-inner))]
                        segments))

               (= (first l) ::context)
               (recur [(conj (pop outer) (into outer-context (partition-all 2) (next l)))
                       (conj (pop inner) (into inner-context (partition-all 2) (next r)))]
                      segments)

               :else
               (let [[l-op & l-args :as left] l
                     [r-op & r-args] (if (nil? r) left r)
                     new-outer (path-segment outer (assoc outer-context :op l-op :start-transform (:end-transform (peek outer))) l-args)
                     new-inner (path-segment inner (assoc inner-context :op r-op :start-transform (:end-transform (peek outer))) r-args)]
                 (recur [new-outer new-inner]
                        segments))))))))

(defmethod path-segment ::left
  [ret {:keys [fn shape start-transform gap] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw r)
               (u/go-forward d)
               (u/yaw (- angle r)))]
    (conj ret (assoc ctx :end-transform tf :model (when-not gap part)))))

(defmethod path-segment ::right
  [ret {:keys [fn shape gap start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 u/pi 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw (- r))
               (u/go-forward d)
               (u/yaw (- (- angle r))))]
    (conj ret (assoc ctx :end-transform tf :model (when-not gap part)))))

(defmethod path-segment ::up
  [ret {:keys [fn shape gap start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (/ u/pi 2) 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/pitch r)
               (u/go-forward d)
               (u/pitch (- angle r)))]
    (conj ret (assoc ctx :end-transform tf :model (when-not gap part)))))

(defmethod path-segment ::down
  [ret {:keys [fn shape gap start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)
                 gap gap}}] args
        degrees  (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [0 (- (/ u/pi 2)) 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf  (-> start-transform
                (u/pitch (- r))
                (u/go-forward d)
                (u/pitch (- (- angle r))))]
    (conj ret (assoc ctx :end-transform tf :model (when-not gap part)))))

(defmethod path-segment ::forward
  [ret {:keys [fn shape start-transform gap] :as ctx} args]
  (let [
        [& {:keys [length model twist gap mask] :or {gap gap}}]  args
        part (binding [m/*fn* fn]
               (as-> (if model
                       model
                       (->> shape
                            (m/extrude-linear {:height length :center false :twist twist}))) m
                 (m/rotatec [(- (u/half u/pi)) 0 0] m)
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (u/go-forward start-transform length)]
    (conj ret (assoc ctx :end-transform tf :model (when-not gap part)))))

(defmethod path-segment ::roll
  [ret {:keys [start-transform] :as ctx} [& {:keys [angle] :or {angle (/ Math/PI 2)}}]]
  (let [new-axes (u/roll start-transform angle)]
    (conj ret (assoc ctx :end-transform new-axes))))

(defmethod path-segment ::hull
  [ret {:keys [fn] :as ctx} [& {:keys [n-segments] :or {n-segments 2}}]]
  (let [hull-segments (into () (take n-segments) (map peek (iterate pop ret)))
        other-forms (nth (iterate pop ret) n-segments)
        model (binding [m/*fn* fn]
                (column->model hull-segments m/hull false))]
    (conj other-forms (assoc ctx
                             :model model
                             :start-transform (:start-transform (first hull-segments))
                             :end-transform (:end-transform (last hull-segments))))))

(defmethod path-segment ::minkowski
  [ret {:keys [fn form] :as ctx} [& {:keys [shape]}]]
  (conj ret (assoc ctx :shape (m/minkowski shape (:shape ctx)) :model nil)))

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

(defn minkowski [& args]
  `(::minkowski ~@args))

(defn join-segments [a b]
  (let [model (column->model [a b] m/union false)]))

(defn look-at-segment [model n]
  (let [segments (-> model meta :segments)
        segment (nth (nth segments 0) n)
        f (u/->inverse-scad-transform (:end-transform segment) u/identity-mat)]
    (f model)))

#_(defn conjoin-models [a b]
    (let [end-tf (-> b meta :end-transform)
          f (u/->inverse-scad-transform end-tf u/identity-mat)]
      (m/union a (f b))))

(defmacro defmodel [name ctx path-spec]
  `(def ~name
     (binding [m/*fn* (:fn ~ctx 10)]
       (path ~ctx ~path-spec))))

(comment

  (-> (path {:curve-radius 20 :fn 70}
            [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
             (up)
             (down)
             (up)
             (left)])
      (look-at-segment 4))

  )
