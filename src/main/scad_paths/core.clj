(ns scad-paths.core
  (:require
   [clojure.walk :refer [postwalk]]
   [scad-clj.scad :as s]
   [scad-paths.transforms :as tr]
   [scad-paths.segment :as sg]
   [scad-paths.triangles :as triangles]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-form (fn [_ args] (:op args)))

(defn unnest [m]
  (if (and (sequential? m)
           (contains? #{:union :difference :intersection} (first m)))
    (loop [[x & xs] m
           ret []]
      (if (nil? x)
        (seq ret)
        (if (sequential? x)
          (if (= (first ret) (first x))
            (recur (concat xs (next x)) ret)
            (if (sequential? (first x))
              (recur (concat xs x) ret)
              (recur xs (conj (unnest x)))))
          (recur xs (conj ret (unnest x))))))
    m))

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
                  (cond-> segs
                    branch (concat (->> branch
                                        (map meta)
                                        (map :models)
                                        (mapcat vals)
                                        (apply concat))))
                  (if branch
                    (conj ret (sg/project seg))
                    (conj ret (sg/project seg))))))))))

(defn ->model [models path-spec transforms]
  (let [segs (mapcat #(transform-segments % identity true) (vals models))
        segments (->> segs
                      (group-by (juxt (comp :order meta) (comp :mask? meta)))
                      (sort-by key))]
    (with-meta
      (reduce (fn [ret [[_ mask?] models]]
                (if mask?
                  (m/difference ret (m/union models))
                  (m/union ret (m/union models))))
              (m/union)
              segments)
      {:segments segments
       :transforms transforms
       :segment-groups (group-by #(get (meta %) :name :unnamed) segs)
       :models models
       :path-spec path-spec})))

(defn lookup-transform
  [model name]
  (or (-> model meta :segment-groups name peek meta :end-transform)
      (-> model meta :transforms name)))

(defn replace-fn [n x]
  (if (and (map? x)
           (or (:fn x)
               (:r x)
               (:angle x)))
    (assoc x :fn n)
    x))

(defn new-fn [model n]
  (postwalk (partial replace-fn n) model))

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

(defn normalize-segment [segment]
  (if (sequential? (ffirst segment))
    (first segment)
    segment))

(defn path
  ([path-forms] (path {:models {}
                       :env {}
                       :transforms {}
                       :scope []
                       :index -1}
                      path-forms))
  ([state path-forms]
   (loop [{:keys [models index] :as state} state
          [form & forms] path-forms]
     (cond (nil? form)
           (->model models path-forms (:transforms state))

           (sequential? form)
           (if (= (first form) ::segment)
             (recur
              state (concat (or (-> form second meta :path-spec) (normalize-segment (next form))) forms))
             (let [args (parse-args form)
                   new-state (path-form (update state :index inc) args)]
               (recur new-state forms)))))))

(defmethod path-form ::model
  [ret args]
  (let [last-model (when-let [m (get (:models ret) (:last-model ret))]
                     (meta (peek m)))
        model (into (or last-model default-model) args)
        model (cond-> model
                (:shape model) (update :shape new-fn (or (:fn args) (:fn model))))
        model-name (:name model)
        existing-model (-> ret :models model-name)]
    (if existing-model
      (update ret :models assoc (:name model)
              (conj (pop existing-model)
                    (let [m (peek existing-model)]
                      (cond-> (vary-meta m merge args)
                        (:shape args) (vary-meta  update :shape new-fn (or (:fn args) (:fn (meta (peek existing-model)))))
                        last-model (vary-meta merge  {:start-transform (:start-transform last-model)
                                                      :end-transform (:end-transform last-model)})))))
      (-> ret
          (update :models assoc model-name [(with-meta (m/union) model)])
          (assoc :last-model model-name)))))

(defmethod path-form ::branch
  [{:keys [models index] :as state} args]
  (let [from-model (:from args)
        model (get models from-model)
        m (path (-> state
                    (update :models assoc from-model (conj (pop model) (vary-meta (peek model) dissoc :branch)))
                    (assoc :index -1)
                    (update :scope conj index))
                (::list args))]
    (-> m meta :models from-model peek meta :branch)
    (assoc state
           :models
           (assoc models
                  from-model
                  (conj (pop model) (vary-meta (peek model) update :branch conj m))))))

(defmethod path-form ::save-transform
  [state args]
  (let [model-name (:model args)
        model (get (:models state) model-name)
        transform-name (:name args)]
    (update state :transforms assoc transform-name (-> model peek meta :end-transform))))

(defn update-models [{:keys [models] :as state} {:keys [to gap] :as args} f]
  (assoc state
         :models
         (reduce
          conj
          models
          (map (fn [[name model]]
                 (let [m-meta (meta (peek model))
                       new-args (if (:shape args)
                                  (update args :shape new-fn (:fn m-meta))
                                  args)
                       m (f (if (:fn new-args)
                              (conj (pop model) (vary-meta (peek model) update :shape new-fn (:fn new-args)))
                              model)
                            (cond-> (assoc m-meta
                                           :start-transform
                                           (:end-transform (meta (peek model))))
                              (:name new-args) (assoc :name (:name new-args))
                              (:fn new-args) (update :shape new-fn (:fn new-args))
                              (:order new-args) (assoc :order (:order new-args)))
                            (dissoc new-args :to :gap))]
                   [name (let [m (if gap
                                   (conj (-> m pop pop) (vary-meta (-> m pop peek)
                                                                   assoc
                                                                   :end-transform
                                                                   (:end-transform (meta (peek m)))))
                                   m)]
                           (conj (pop m)
                                 (vary-meta
                                  (peek m)
                                  (fn [mta]
                                    (dissoc
                                     (if (:name new-args)
                                       (assoc mta :name (:name new-args))
                                       (dissoc mta :name))
                                     :branch)))))]))
               (if to (select-keys models to) models)))))

(defmacro def-segment-handler [key & func]
  `(defmethod path-form ~key
     [state# args#]
     (update-models state# args# (fn ~@func))))

(def-segment-handler ::set
  [ret _ args]
  (conj (pop ret) (vary-meta (peek ret) merge args)))

(def-segment-handler ::left
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> shape
                    (m/rotatec [Math/PI 0 0])
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
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
         angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
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
  (let [{:keys [curve-radius angle gap side-length]
         :or {curve-radius (:curve-radius ctx)
              gap gap}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
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
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
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
  (let [{:keys [length model twist mask center]} args
        shape (if (and (:fn args) (not= (:fn ctx) (:fn args)))
                (new-fn shape (:fn args))
                shape)
        part (m/with-fn fn
               (as-> (if model
                       (new-fn model (or (:fn args) (:fn ctx)))
                       (->> shape
                            (m/extrude-linear {:height length :center center :twist twist}))) m
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (u/go-forward start-transform (cond-> length center (/ 2)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(def-segment-handler ::offset
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length offset]} args
        shape (m/offset offset
                        (if (and (:fn args) (not= (:fn ctx) (:fn args)))
                          (new-fn shape (or (:fn args) (:fn ctx)))
                          shape))
        part (m/with-fn fn
               (->> shape
                    (m/extrude-linear {:height length :center false})))
        tf (u/go-forward start-transform length)]
    (conj ret (with-meta part (assoc ctx :end-transform tf :shape shape)))))

(def-segment-handler ::backward
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length model twist mask center]}  args
        part (binding [m/*fn* fn]
               (as-> (if model
                       (new-fn model (or (:fn args) (:fn ctx)))
                       (->> shape
                            (m/extrude-linear {:height length :center center :twist twist})
                            (m/translate [0 0 (- length)]))) m
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (u/go-backward start-transform (cond-> length center (/ 2)))]
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
  [ret {:keys [start-transform]} {:keys [axis angle x y z] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [axis (if x :x (if y :y (if z :z axis)))
        angle (or x y z angle)
        seg (vary-meta (peek ret) assoc :end-transform (u/rotate start-transform axis angle))]
    (conj (pop ret) seg)))

(def-segment-handler ::transform
  [ret _ {:keys [transform] :or {transform u/identity-mat}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform transform)]
    (conj (pop ret) seg)))

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

(def-segment-handler ::arc
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius side-length]
         :or {curve-radius (:curve-radius ctx)
              side-length 10}} args
        angle (triangles/abc->A side-length curve-radius curve-radius)
        degrees (* angle 57.29578)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        part (binding [m/*fn* fn]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ Math/PI 2) 0 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        tf (-> start-transform
               (u/yaw (- r))
               (u/go-forward d)
               (u/yaw (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(defn left [& opts]
  `(::left ~@opts))

(defn right [& opts]
  `(::right ~@opts))

(defn up [& opts]
  `(::up ~@opts))

(defn down [& opts]
  `(::down ~@opts))

(defn arc [& opts]
  `(::arc ~@opts))

(defn roll [& opts]
  `(::roll ~@opts))

(defn forward [& opts]
  `(::forward ~@opts))

(defn backward [& opts]
  `(::backward ~@opts))

(defn hull [& opts]
  `(::hull ~@opts))

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

(defn set [& args]
  `(::set ~@args))

(defn branch [& args]
  `(::branch ~@args))

(defn segment [& args]
  `(::segment ~@args))

(defn to [& args]
  `(::to ~@args))

(defn mask [& args]
  `(::model (conj (vec args) :mask? true)))

(defn save-transform [& args]
  `(::save-transform ~@args))

(defn offset [& args]
  `(::offset ~@args))

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
