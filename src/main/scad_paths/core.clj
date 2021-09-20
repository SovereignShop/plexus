(ns scad-paths.core
  (:require
   [scad-clj.scad :as s]
   [scad-paths.segment :as sg]
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
            [seg & segs] segments
            ret []]
       (let [{:keys [branch]} (meta seg)]
         (if (nil? seg)
           (with-meta
             (if (not join-branch?) (inverse-tf (join-op ret)) (join-op ret))
             (assoc segment-data
                    :segments segments
                    :start-transform start-tf))
           (recur (meta seg)
                  segs
                  (cond-> (conj ret (sg/project seg))
                    (and branch join-branch?) (conj (join-segments branch join-op join-branch?))))))))))

(defn ->model [segments path-spec]
  (let [[outer-section inner-section] (map join-segments segments)]
    (with-meta
      (m/difference outer-section inner-section)
      {:segments segments
       :path-spec path-spec
       :start-transform (-> outer-section meta :start-transform)
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
                     end-tf-outer (:end-transform (meta (peek outer)))
                     end-tf-inner (:end-transform (meta (peek inner)))
                     l-opts (into {} (partition-all 2) l-args)
                     r-opts (into {} (partition-all 2) r-args)
                     new-outer (path-segment outer (assoc (meta outer-segment) :op l-op :start-transform end-tf-outer) l-opts)
                     new-inner (path-segment inner (assoc (meta inner-segment) :op r-op :start-transform end-tf-inner) r-opts)]
                 (recur [(if (:gap l-opts)
                           (conj (pop outer) (with-meta (peek outer) (meta (peek new-outer))))
                           new-outer)
                         (if (:gap r-opts)
                           (conj (pop inner) (with-meta (peek inner) (meta (peek new-inner))))
                           new-inner)]
                        segments))))))))

(defmethod path-segment ::left
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle]
         :or {curve-radius (:curve-radius ctx)
              angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ Math/PI 2) 0 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw (- r))
               (u/go-forward d)
               (u/yaw (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::right
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle]
         :or {curve-radius (:curve-radius ctx)
              angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(- (/ u/pi 2)) u/pi 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw r)
               (u/go-forward d)
               (u/yaw (- angle r)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::up
  [ret {:keys [fn shape gap start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle gap]
         :or {curve-radius (:curve-radius ctx)
              angle (/ Math/PI 2)
              gap gap}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ u/pi 2) 0 (/ u/pi 2)])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/pitch r)
               (u/go-forward d)
               (u/pitch (- angle r)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::down
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle]
         :or {curve-radius (:curve-radius ctx)
              angle (/ Math/PI 2)}} args
        degrees  (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ u/pi 2) 0 (- (/ u/pi 2))])))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf  (-> start-transform
                (u/pitch (- r))
                (u/go-forward d)
                (u/pitch (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::forward
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length model twist mask]} args
        part (binding [m/*fn* fn]
               (as-> (if model
                       model
                       (->> shape
                            (m/extrude-linear {:height length :center false :twist twist}))) m
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (u/go-forward start-transform length)]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::backward
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length model twist mask]}  args
        part (binding [m/*fn* fn]
               (as-> (if model
                       model
                       (->> shape
                            (m/extrude-linear {:height length :center false :twist twist})
                            (m/translate [0 0 (- length)]))) m
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (u/go-backward start-transform length)]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defmethod path-segment ::roll
  [ret _ {:keys [angle] :or {angle (/ Math/PI 2)}}]
  (conj (pop ret) (vary-meta (peek ret) assoc :end-transform (u/roll (:end-transform (meta (peek ret))) angle))))

(defmethod path-segment ::hull
  [ret {:keys [fn]} {:keys [n-segments] :or {n-segments 2}}]
  (let [hull-segments (into () (take n-segments) (map peek (iterate pop ret)))
        other-forms (nth (iterate pop ret) n-segments)
        new-segment (binding [m/*fn* fn]
                      (join-segments hull-segments m/hull false))]
    (conj other-forms new-segment)))

(defmethod path-segment ::translate
  [ret _ {:keys [x y z] :or {x 0 y 0 z 0}}]
  (let [seg (sg/set-translation (peek ret) [x y z])]
    (conj (pop ret) seg)))

(defmethod path-segment ::no-op
  [ret _ _]
  ret)

(defn no-op [& opts]
  `(::no-op ~@opts))

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

(defn backward [& opts]
  `(::backward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))

(defn context [& args]
  `(::context ~@args))

(defn model [& args]
  `(::model ~@args))

(defn translate [& args]
  `(::translate ~@args))

(defmacro defmodel [name ctx path-spec]
  `(def ~name
     (binding [m/*fn* (:fn ~ctx 10)]
       (path ~ctx ~path-spec))))
