(ns scad-paths.core
  (:require
   [scad-clj.scad :as s]
   [scad-paths.transforms :as tr]
   [scad-paths.segment :as sg]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-form (fn [_ args] (:op args)))

(defn transform-segments
  ([segments] (transform-segments segments m/union true))
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
                    (and branch join-branch?) (conj (transform-segments branch join-op join-branch?))))))))))

(defn ->model [models path-spec]
  (let [segments (->> (mapcat #(transform-segments % identity true) (vals models))
                      (group-by (juxt (comp :order meta) (comp :mask? meta)))
                      (sort-by key))]
    (with-meta
      (reduce (fn [ret [[_ mask?] models]]
                (if mask?
                  (m/difference ret (m/union models) )
                  (m/union ret (m/union models))))
              (m/union)
              segments)
      {:segments segments
       :models models
       :path-spec path-spec})))

(defn lookup-transform
  [model name]
  (-> model meta :connections name))

(def default-model
  {:curve-radius 7
   :fn 10
   :order 0
   :mask? false
   :name :default
   :start-transform u/identity-mat
   :end-transform u/identity-mat})

(defn parse-args [form]
  (loop [[arg & args] (next form)
         kvs (transient {:op (first form)})]
    (if (nil? arg)
      (persistent! kvs)
      (if (keyword? arg)
        (recur (next args)
               (assoc! kvs arg (first args)))
        (persistent! (assoc! kvs ::list (vec (cons arg args))))))))

(defn path
  ([path-forms] (path {:models {}
                       :env {}
                       :transforms {}}
                      path-forms))
  ([state path-forms]
   (loop [{:keys [models] :as state} state
          [form & forms] path-forms]
     (cond (nil? form)
           (->model models path-forms)

           (sequential? form)
           (if (= (first form) ::segment)
             (recur state (concat (or (-> form second meta :path-spec) (next form)) forms))
             (let [args (parse-args form)
                   new-state (path-form state args)]
               (recur new-state forms)))))))

(defmethod path-form ::context
  [ret _ args]
  (conj (pop ret) (vary-meta (peek ret) into args)))

(defmethod path-form ::model
  [ret args]
  (let [model (into default-model args)]
    (update ret :models assoc (:name model) [(with-meta (m/union) model)])))

(defmethod path-form ::branch
  [{:keys [models] :as state} args]
  (let [m (path state (::list args))]
    (assoc models
           :models
           (into {}
                 (map (fn [[name model]]
                        [name (conj (pop model) (vary-meta (peek model) assoc :branch (-> m meta :models name)))]))
                 models))))

(defn update-models [{:keys [models] :as state} {:keys [to] :as args} f]
  (assoc state
         :models
         (reduce
          conj
          models
          (map (fn [[name model]]
                 [name (f model
                          (cond-> (assoc (meta (peek model))
                                         :start-transform
                                         (:end-transform (meta (peek model))))
                            (:order args) (assoc :order (:order args)))
                          (dissoc args :to))])
               (if to (select-keys models to) models)))))

(defmacro def-segment-handler [key & func]
  `(defmethod path-form ~key
     [state# args#]
     (update-models state# args# (fn ~@func))))

(def-segment-handler ::left
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

(def-segment-handler ::right
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

(def-segment-handler ::up
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

(def-segment-handler ::down
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

(def-segment-handler ::forward
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

(def-segment-handler ::backward
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

(def-segment-handler ::roll
  [ret _ {:keys [angle] :or {angle (/ Math/PI 2)}}]
  (conj (pop ret) (vary-meta (peek ret) assoc :end-transform (u/roll (:end-transform (meta (peek ret))) angle))))

(def-segment-handler ::hull
  [ret {:keys [fn]} {:keys [n-segments] :or {n-segments 2}}]
  (let [hull-segments (into () (take n-segments) (map peek (iterate pop ret)))
        other-forms (nth (iterate pop ret) n-segments)
        new-segment (binding [m/*fn* fn]
                      (transform-segments hull-segments m/hull false))]
    (conj other-forms new-segment)))

(def-segment-handler ::translate
  [ret {:keys [start-transform]} {:keys [x y z]}]
  (let [tf (cond-> start-transform
             x (u/go-forward x :x)
             y (u/go-forward y :y)
             z (u/go-forward z :z))]
    (conj (pop ret) (vary-meta (peek ret) assoc :end-transform tf))))

(def-segment-handler ::rotate
  [ret {:keys [start-transform]} {:keys [axis angle] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform (u/rotate start-transform axis angle))]
    (conj (pop ret) seg)))

(def-segment-handler ::transform
  [ret _ {:keys [transform] :or {transform u/identity-mat}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform transform)]
    (conj (pop ret) seg)))

(def-segment-handler ::no-op
  [ret _ _]
  ret)

(def-segment-handler ::spin
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

(defn model [& args]
  `(::model ~@args))

(defn branch [& args]
  `(::branch ~@args))

(defn segment [& args]
  `(::segment ~@args))

(defn parse-path [path-spec]
  (loop [[x & xs] path-spec
         args {}]
    (cond (nil? x)
          [args nil]

          (keyword? x)
          (recur (next xs) (assoc args x (first xs)))

          :else
          [args (vec (cons x xs))])))

(defmacro defmodel [name & path]
  (let [[opts path] (parse-path path)]
    `(do (binding [m/*fn* ~(get opts :fn 10)]
           (def ~name
             (path ~path)))
         ~name)))
