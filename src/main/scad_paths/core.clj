(ns scad-paths.core
  (:require
   [scad-clj.scad :as s]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-segment (fn [_ ctx _] (:op ctx)))

(defn join-segments
  ([segments] (join-segments segments m/union true))
  ([segments join-op join-branch?]
   (let [first-segment-data (meta (first segments))
         start-tf (:start-transform first-segment-data)
         inverse-tf (u/->inverse-scad-transform start-tf u/identity-mat)]
     (loop [segment-data first-segment-data
            [seg & segments] segments
            ret []]
       (let [{:keys [start-transform branch]} (meta seg)]
         (if (nil? seg)
           (with-meta
             (if (not join-branch?) (inverse-tf (join-op ret)) (join-op ret))
             (assoc segment-data
                    :segments segments
                    :start-transform start-tf))
           (let [f (u/->scad-transform u/identity-mat start-transform)]
             (recur (meta seg)
                    segments
                    (cond-> (conj ret (f seg))
                      (and branch join-branch?) (conj (join-segments branch join-op join-branch?)))))))))))

(defn ->model [segments path-spec]
  (let [[outer-section inner-section] (map join-segments segments)]
    (with-meta
      (m/difference outer-section inner-section)
      {:segments segments
       :path-spec path-spec
       :start-transform (-> outer-section meta :start-transform)
       :end-transform (-> outer-section meta :end-transform)
       :outer-model outer-section
       :inner-model inner-section})))

(defn path
  ([path-spec]
   (path nil path-spec))
  ([ctx path-spec]
   (path ctx ctx path-spec))
  ([outer-segment inner-segment path-spec]
   (let [default-context {:curve-radius 7
                          :fn 10
                          :shape (binding [m/*fn* (:fn outer-segment 10)]
                                   (m/circle 5))
                          :start-transform u/identity-mat
                          :end-transform u/identity-mat}]
     (loop [ret [[(with-meta (m/union) (into default-context outer-segment))]
                 [(with-meta (m/union) (into default-context inner-segment))]]
            [seg & segments] path-spec]
       (let [[l r] (if (vector? seg) seg [seg])
             outer (nth ret 0)
             outer-segment (peek outer)
             inner (nth ret 1)
             inner-segment (peek inner)]
         (cond (nil? l)
               (->model ret path-spec)

               (= l :segment)
               (recur ret (into (or (-> r meta :path-spec) r) segments))


               (= l :branch)
               (let [model (path (meta outer-segment) (meta inner-segment) (:path-spec (meta r) r))
                     [branch-outer branch-inner] (-> model meta :segments)]
                 (recur [(conj (pop outer) (vary-meta outer-segment assoc :branch branch-outer))
                         (conj (pop inner) (vary-meta inner-segment assoc :branch branch-inner))]
                        segments))

               (= (first l) ::context)
               (recur [(conj (pop outer) (vary-meta outer-segment into (partition-all 2) (next l)))
                       (conj (pop inner) (vary-meta inner-segment into (partition-all 2) (next r)))]
                      segments)

               :else
               (let [[l-op & l-args :as left] l
                     [r-op & r-args] (if (nil? r) left r)
                     new-outer (path-segment outer (assoc (meta outer-segment) :op l-op :start-transform (:end-transform (meta (peek outer)))) l-args)
                     new-inner (path-segment inner (assoc (meta inner-segment) :op r-op :start-transform (:end-transform (meta (peek outer)))) r-args)]
                 (recur [new-outer new-inner]
                        segments))))))))

(defmethod path-segment ::left
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)}}] args
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
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::right
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)}}] args
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
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

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
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::down
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [[& {:keys [curve-radius angle gap]
            :or {curve-radius (:curve-radius ctx)
                 angle (/ Math/PI 2)}}] args
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
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

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
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::roll
  [ret {:keys [start-transform] :as ctx} [& {:keys [angle] :or {angle (/ Math/PI 2)}}]]
  (let [new-axes (u/roll start-transform angle)]
    (conj ret (assoc ctx :end-transform new-axes))))

(defmethod path-segment ::hull
  [ret {:keys [fn]} [& {:keys [n-segments] :or {n-segments 2}}]]
  (let [hull-segments (into () (take n-segments) (map peek (iterate pop ret)))
        other-forms (nth (iterate pop ret) n-segments)
        new-segment (binding [m/*fn* fn]
                      (join-segments hull-segments m/hull false))]
    (conj other-forms new-segment)))

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

(defn look-at-segment [model n]
  (let [segments (-> model meta :segments)
        segment (nth (nth segments 0) n)
        f (u/->inverse-scad-transform (:end-transform segment) u/identity-mat)]
    (f model)))

(defmacro defmodel [name ctx path-spec]
  `(def ~name
     (binding [m/*fn* (:fn ~ctx 10)]
       (path ~ctx ~path-spec))))

(comment

  (defmodel hull-test
    {:curve-radius 20 :fn 70}
    [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
     (forward :length 10)
     [(context :shape (m/circle 10)) (context :shape (m/circle 8))]
     (forward :length 10)
     (hull)
     [(context :shape (m/circle 6)) (context :shape (m/circle 4))]
     (forward :length 10)
     (hull)])

  (path {:curve-radius 10 :fn 10}
        [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
         [:branch
          [(left) (right) (forward :length 20)]]
         [:branch
          [(right) (left) (forward :length 20)]]])

  )
