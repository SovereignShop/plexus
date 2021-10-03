(ns scad-paths.core
  (:require
   [scad-clj.scad :as s]
   [scad-paths.transforms :as tr]
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
  (let [outer-section (join-segments (segments 0))
        inner-section (if-let [seg (get segments 1)]
                        (join-segments seg)
                        (m/union))]
    (with-meta
      (m/difference outer-section inner-section)
      {:segments segments
       :path-spec path-spec
       :start-transform (-> outer-section meta :start-transform)
       :outer-model outer-section
       :inner-model inner-section})))

(defn lookup-transform
  [model name]
  (-> model meta :connections name))

(defn path
  ([forms]
   (path [{}] forms))
  ([contexts path-forms]
   (let [default-context {:curve-radius 7
                          :fn 10
                          :start-transform u/identity-mat
                          :end-transform u/identity-mat}]
     (loop [segments (if (vector? contexts)
                       (into {}
                             (for [[i ctx] (map-indexed list contexts)]
                               [i [(with-meta (m/union) (into default-context ctx))]]))
                       contexts)
            connection-transforms {}
            [form & forms] path-forms]
       (let [form (if (vector? form) form [form])]
         (cond (nil? (first form))
               (->model segments path-forms)

               (= (first form) :segment)
               (recur segments connection-transforms (into (or (-> (second form) meta :path-spec) (second form)) forms))

               (= (first form) :branch)
               (let [r (second form)
                     model (path segments (:path-spec (meta r) r))
                     branch-segments (-> model meta :segments)]
                 (recur (merge-with (fn [ctx branch]
                                      (conj (pop ctx) (vary-meta (peek ctx) assoc :branch branch)))
                                    segments
                                    branch-segments)
                        (merge connection-transforms (:connections (meta model)))
                        forms))

               :else
               (let [segments (if (< (count segments) (count form))
                                (assoc segments (count segments) (get segments (dec (count segments))))
                                segments)
                     segments
                     (into segments
                           (for [[idx column] segments]
                             (let [clause (if (contains? form idx)
                                            (nth form idx)
                                            (last form))
                                   [op & args] clause
                                   opts (into {} (partition-all 2) args)
                                   last-segment (peek column)
                                   end-tf (:end-transform (meta last-segment))
                                   new-column (path-segment column (assoc (meta last-segment) :op op :start-transform end-tf) opts)]
                               [idx (if (:gap opts)
                                      (conj (pop column) (with-meta (peek column) (meta (peek new-column))))
                                      new-column)])))]
                 (recur segments
                        connection-transforms
                        forms))))))))

(defmethod path-segment ::context
  [ret _ args]
  (conj (pop ret) (vary-meta (peek ret) into args)))

(defmethod path-segment ::left
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle]
         :or {curve-radius (:curve-radius ctx)
              angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/rotatec [0 0 Math/PI])
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
                    (m/rotatec [0 0 (- (/ Math/PI 2))])
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
                    (m/rotatec [0 0 (/ Math/PI 2)])
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
  [ret {:keys [start-transform]} {:keys [x y z]}]
  (let [tf (cond-> start-transform
             x (u/go-forward x :x)
             y (u/go-forward y :y)
             z (u/go-forward z :z))]
    (conj (pop ret) (vary-meta (peek ret) assoc :end-transform tf))))

(defmethod path-segment ::rotate
  [ret {:keys [start-transform]} {:keys [axis angle] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform (u/rotate start-transform axis angle))]
    (conj (pop ret) seg)))

(defmethod path-segment ::transform
  [ret _ {:keys [transform] :or {transform u/identity-mat}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform transform)]
    (conj (pop ret) seg)))

(defmethod path-segment ::no-op
  [ret _ _]
  ret)

(defmethod path-segment ::spin
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [angle]
         :or {angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> (m/difference
                     shape
                     (->> (m/square 1000 1000)
                          (m/translate [-500 0])))
                    (m/extrude-rotate {:angle degrees})))]
    (conj ret (with-meta part (assoc ctx :end-transform start-transform)))))

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

(defn ctx [& args]
  `(::context ~@args))

(defn model [& args]
  `(::model ~@args))

(defn translate [& args]
  `(::translate ~@args))

(defn rotate [& args]
  `(::rotate ~@args))

(defn transform [& args]
  `(::transform ~@args))

(defn spin [& args]
  `(::spin ~@args))

(defmacro defmodel [name ctx path-spec]
  `(do (def ~name
         (binding [m/*fn* (:fn ~ctx 10)]
           (path [~ctx] ~path-spec)))
       ~name))
