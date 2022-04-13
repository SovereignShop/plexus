(ns scad-paths.core
  (:refer-clojure :exclude [set])
  (:require
   [clojure.core.matrix :as mat]
   [clojure.walk :refer [postwalk]]
   [scad-clj.scad :as s]
   [scad-paths.transforms :as tr]
   [scad-paths.segment :as sg]
   [scad-paths.triangles :as triangles]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]))

(defmulti path-form (fn [_ args] (:op args)))

(defn parse-path [path-spec]
  (loop [[x & xs] path-spec
         args {}]
    (cond (nil? x)
          [args nil]

          (keyword? x)
          (recur (next xs) (assoc args x (first xs)))

          :else
          [args (vec (cons x xs))])))

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
                    branch (concat (->> branch ;; Needs re-write. Transform-segments needs to be state -> state,
                                               ;; building up segments.
                                        (map meta)
                                        (map :models)
                                        (mapcat vals)
                                        (apply concat))))
                  (if branch
                    (conj ret (sg/project seg))
                    (conj ret (sg/project seg))))))))))

(defn result-tree->model
  [{:keys [models result] :as state}]
  (let [segs (mapcat #(transform-segments % identity true)
                     (vals models))
        segments (group-by (comp :name meta) segs)
        f (fn walk
            ([tree]
             (if (keyword? tree)
               (apply m/union (get segments tree))
               (case (first tree)
                 ::union (apply m/union (sequence (comp (remove nil?) (map walk)) (next tree)))
                 ::difference (apply m/difference (sequence (comp (remove nil?) (map walk)) (next tree)))
                 ::intersection (apply m/intersection (sequence (comp (remove nil?) (map walk)) (next tree)))))))
        ret (f result)]
    (with-meta ret state)))

(defn ->model [{:keys [models result path-spec transforms name] :as state}]
  (if result
    (result-tree->model state)
    (let [segs (mapcat #(transform-segments % identity true)
                       (vals models))
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
         :name (or name "default")
         :segment-groups (group-by #(get (meta %) :name :unnamed) segs)
         :models models
         :path-spec path-spec}))))

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
   :shape nil
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
  (remove nil?
          (if (sequential? (ffirst segment))
            (first segment)
            segment)))

(def default-state
  {:models {}
   :transforms {}
   :scope []
   :index -1})

(defn path*
  ([path-forms] (path* default-state
                      path-forms))
  ([state path-forms]
   (loop [{:keys [models index] :as state} state
          [form & forms] (remove nil? path-forms)]
     (cond (nil? form)
           (->model (assoc state :path-spec path-forms))

           (sequential? form)
           (if (= (first form) ::segment)
             (recur
              state
              (concat (or (-> form second meta :path-spec) (normalize-segment (next form)))
                      forms))
             (let [args (parse-args form)
                   new-state (path-form (update state :index inc) args)]
               (recur new-state forms)))))))

(defn path [& path-forms]
  (path* path-forms))

(defmethod path-form ::model
  [ret args]
  (let [last-model (when-let [m (get (:models ret) (:last-model ret))]
                     (meta (peek m)))
        model (into (or (dissoc last-model :shape) default-model) args)
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
  [{:keys [models index transforms] :as state} args]
  (let [from-model (:from args)
        model (get models from-model)
        m (path* (-> state
                     (update :models assoc from-model (conj (pop model) (vary-meta (peek model) dissoc :branch)))
                     (assoc :index -1)
                     (update :scope conj index))
                 (::list args))]
    (assoc state
           :models
           (assoc models
                  from-model
                  (conj (pop model) (vary-meta (peek model) update :branch conj m)))
           :transforms (merge transforms (-> m meta :transforms)))))

(defn ->keyword [namespace name*]
  (keyword (name namespace) (name name*)))

(defmethod path-form ::save-transform
  [state args]
  (let [model-name (:model args)
        model (get (:models state) model-name)
        model-state (-> model peek meta)
        namespace (:namespace model-state)
        transform-name (cond->> (:name args)
                         namespace (->keyword namespace))]
    (update state :transforms assoc transform-name (:end-transform model-state))))

(defmethod path-form ::result
  [state {tree ::list}]
  (assoc state :result (first tree)))

(defn update-models [{:keys [models ignored-models] :as state} {:keys [to gap skip op] :as args} f]
  (let [gap-models (clojure.core/set
                    (if (boolean? gap)
                      (or to (keys models))
                      gap))]
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
                                             :name name
                                             :start-transform
                                             (:end-transform (meta (peek model))))
                                (:name new-args) (assoc :name (:name new-args))
                                (:fn new-args) (update :shape new-fn (:fn new-args))
                                (:order new-args) (assoc :order (:order new-args)))
                              (dissoc new-args :to :gap))]
                     [name (let [m (if (contains? gap-models name)
                                     (conj (-> m pop pop) (vary-meta (-> m pop peek)
                                                                     assoc
                                                                     :end-transform
                                                                     (:end-transform (meta (peek m)))))
                                     m)]
                             (conj (pop m)
                                   (vary-meta
                                    (peek m)
                                    (fn [mta]
                                      (cond-> (if (:name new-args)
                                                (assoc mta :name (:name new-args))
                                                mta
                                                #_(dissoc mta :name))
                                        (> (count m) (count model))
                                        (dissoc :branch))))))]))
                 (reduce dissoc
                         (if to (select-keys models to) models)
                         ignored-models))))))

(defmethod path-form ::ignore
  [state {:keys [models]}]
  (assoc state :ignored-models models))

(defmacro def-segment-handler [key & func]
  `(defmethod path-form ~key
     [state# args#]
     (update-models state# args# (fn ~@func))))

(def-segment-handler ::set
  [ret _ args]
  (conj (pop ret) (vary-meta (peek ret) merge (dissoc args :op))))


(defn left-curve-points [curve-radius curve-angle face-number]
  (let [step-size (/ curve-angle face-number)]
    (for [x (range 0 curve-angle step-size)]
      [(- (* curve-radius (Math/cos x)) curve-radius)
       (* curve-radius (Math/sin x))])))

(def-segment-handler ::left-2d
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        points (left-curve-points curve-radius angle fn)
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/roll r)
               (u/go-forward d :y)
               (u/roll (- angle r)))]
    (conj ret (with-meta (list :points points) (assoc ctx :end-transform tf)))))

(defn all-transforms [start-transform tf-fn curve-radius angle steps]
  (let [step-angle (/ angle steps)]
    (take (inc steps)
          (iterate
           (fn [transform]
             (let [d (u/bAc->a curve-radius step-angle curve-radius)
                   r (- (/ Math/PI 2) (/ (- Math/PI step-angle) 2))]
               (tf-fn transform r d step-angle)))
           start-transform))))

(def-segment-handler ::left
  [ret {:keys [shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (binding [m/*fn* (:fn ctx)]
               (->> shape
                    (m/rotatec [Math/PI 0 0])
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ Math/PI 2) 0 0])))
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (u/yaw (- r))
                                  (u/go-forward d)
                                  (u/yaw (- (- a r)))))
                            curve-radius
                            angle
                            (/ (:fn ctx) 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw (- r))
               (u/go-forward d)
               (u/yaw (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :all-transforms tfs)))))

(def-segment-handler ::right
  [ret {:keys [shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (binding [m/*fn* (:fn ctx)]
               (->> shape
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(- (/ u/pi 2)) u/pi 0])))
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (u/yaw r)
                                  (u/go-forward d)
                                  (u/yaw (- a r))))
                            curve-radius
                            angle
                            (/ (:fn ctx) 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw r)
               (u/go-forward d)
               (u/yaw (- angle r)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :all-transforms tfs)))))

(def-segment-handler ::up
  [ret {:keys [shape gap start-transform name] :as ctx} args]
  (let [{:keys [curve-radius angle gap side-length elevation step-fn]
         :or {curve-radius (:curve-radius ctx)
              elevation 0
              gap gap}} args
        face-number (or (:fn args) (:fn ctx))
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        [new-shape model]
        (binding [m/*fn* face-number]
          (u/extrude-rotate {:angle degrees
                             :face-number face-number
                             :elevation elevation
                             :model name
                             :curve-radius curve-radius
                             :step-fn step-fn}
                            shape))
        part (binding [m/*fn* face-number]
               (->> model
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ u/pi 2) 0 (/ u/pi 2)])))
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (u/pitch r)
                                  (u/go-forward d)
                                  (u/go-forward elevation :x)
                                  (u/pitch (- a r))))
                            curve-radius
                            angle
                            (/ face-number 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/pitch r)
               (u/go-forward d)
               (u/go-forward elevation :x)
               (u/pitch (- angle r)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :shape new-shape :all-transforms tfs)))))

(def-segment-handler ::down
  [ret {:keys [shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length]
         :or {curve-radius (:curve-radius ctx)}} args
        face-number (or (:fn args) (:fn ctx))
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees  (* angle 57.29578)
        part (binding [m/*fn* face-number]
               (->> shape
                    (m/rotatec [0 0 (/ Math/PI 2)])
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ u/pi 2) 0 (- (/ u/pi 2))])))
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (u/pitch (- r))
                                  (u/go-forward d)
                                  (u/pitch (- (- a r)))))
                            curve-radius
                            angle
                            (/ face-number 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf  (-> start-transform
                (u/pitch (- r))
                (u/go-forward d)
                (u/pitch (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :all-transforms tfs)))))

(defn forward-impl* [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length model twist mask center step-length axis]
         :or {step-length length}} args
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
        tf (u/go-forward start-transform (cond-> length center (/ 2)))
        all-transforms (conj (vec (for [step (range (quot length step-length))]
                                    (u/go-forward start-transform (* step step-length))))
                             tf)]
    (conj ret (with-meta part (assoc ctx :end-transform tf :all-transforms all-transforms)))))


(def-segment-handler ::forward
  [ret ctx args]
  (forward-impl* ret ctx args))

(def-segment-handler ::offset
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [length offset]} args
        new-shape  (m/offset offset
                         (if (and (:fn args) (not= (:fn ctx) (:fn args)))
                           (new-fn shape (or (:fn args) (:fn ctx)))
                           shape))
        part (m/with-fn fn
               (cond->> new-shape
                 length (m/extrude-linear {:height length :center false})))
        tf (cond-> start-transform
             length (u/go-forward length))]
    (if length
      (conj ret (with-meta part (assoc ctx :end-transform tf :shape new-shape)))
      (conj (pop ret) (vary-meta (peek ret) assoc :shape new-shape)))))

(def-segment-handler ::minkowski
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [minkowski-shape (:shape args)
        new-shape (m/minkowski minkowski-shape shape)]
    (conj (pop ret) (vary-meta (peek ret) assoc :shape new-shape))))

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
  [ret {:keys [start-transform]} {:keys [x y z global?]}]
  (let [tf (if global?
             (cond-> start-transform
               x (u/set-translation x :x)
               y (u/set-translation y :y)
               z (u/set-translation z :z)
               true (mat/to-nested-vectors))
             (cond-> start-transform
               x (u/go-forward x :x)
               y (u/go-forward y :y)
               z (u/go-forward z :z)))]
    (conj (pop ret) (vary-meta (peek ret) assoc :end-transform tf))))

(defn rotate-impl
  [ret {:keys [start-transform]} {:keys [axis angle x y z] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [axis (if x :x (if y :y (if z :z axis)))
        angle (or x y z angle)
        seg (vary-meta (peek ret) assoc :end-transform (u/rotate start-transform axis angle))]
    (conj (pop ret) seg)))

(def-segment-handler ::rotate
  [ret ctx args]
  (rotate-impl ret ctx args))

(def-segment-handler ::transform
  [ret _ {:keys [transform] :or {transform u/identity-mat}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform transform)]
    (conj (pop ret) seg)))

(def-segment-handler ::spin
  [ret {:keys [fn shape start-transform] :as ctx} args]
  (let [{:keys [angle axis]
         :or {angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> (m/difference
                     (cond->> shape
                       (= axis :y) (m/rotatec [0 0 (/ Math/PI 2)]))
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

(def-segment-handler ::add-ns
  [ret ctx {:keys [namespace]}]
  (if namespace
    (conj (pop ret) (vary-meta (peek ret) update :namespace #(keyword (if %1
                                                                        (str (name %1) "." (name namespace))
                                                                        namespace))))
    ret))

(def-segment-handler ::union
  [ret _ {:keys [shape]}]
  (conj (pop ret) (vary-meta (peek ret) update :shape m/union shape)))

(def-segment-handler ::forward-until
  [ret {:keys [start-transform]} {:keys [x]}]
  (let [rot (u/rotation-matrix start-transform)
        z (nth rot 2)
        x* (nth rot 1)
        dx (- x x*)
        angle (u/angle-between x z)]
    (/ dx (Math/cos angle))
    ret))


(defn pythag-distance [[x1 y1 z1]
                       [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2)
                (Math/pow (- y2 y1) 2)
                (Math/pow (- z2 z1) 2))))

(def-segment-handler ::extrude-to
  [ret {:keys [start-transform] :as ctx} {:keys [target-transform]}]
  (let [extrusion-length (pythag-distance
                          (u/translation-vector target-transform)
                          (u/translation-vector start-transform))
        v (mat/sub
           (u/translation-vector target-transform)
           (u/translation-vector start-transform))
        r0 (u/rotation-matrix start-transform)
        [angle ortho] (u/rotation-axis-and-angle (nth r0 2) v [0 0 1])
        ret (rotate-impl ret ctx {:axis ortho :angle angle})
        ctx (-> ret peek meta)
        ret (forward-impl* ret (assoc ctx :start-transform (:end-transform ctx)) {:length extrusion-length})
        ]
    ret))

(defn extrude-to [& opts]
  `(::extrude-to ~@opts))

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

(defn to [& p]
  (let [[opts parsed-path] (parse-path p)
        path* (map (fn [[x & xs]]
                     (list* x :to (:models opts) xs))
                   parsed-path)]
    (segment path*)))

(defn mask [& args]
  `(::model ~@(conj (vec args) :mask? true)))

(defn body [& args]
  `(::model ~@(concat args [:mask? false])))

(defn save-transform [& args]
  `(::save-transform ~@args))

(defn offset [& args]
  `(::offset ~@args))

(defn minkowski [& args]
  `(::minkowski ~@args))

(defn add-ns [& args]
  `(::add-ns ~@args))

(defn forward-until [& args]
  `(::forward-until ~@args))

(defn ignore [& args]
  `(::ignore ~@args))

(defn result [& args]
  `(::result ~@args))

(defn union [& args]
  `(::union ~@args))

(defn intersection [& args]
  `(::intersection ~@args))

(defn difference [& args]
  `(::difference ~@args))



(defn pattern [& args]
  (let [{:keys [from axis distances angles namespaces end-at ::list]} (parse-args (list* :na args))]
    (assert (or angles distances))
    (apply segment
           (sort-by #(= (first %) ::segment)
                    (for [[angle distance namespace idx]
                          (map vector
                               (or angles (repeat 0))
                               (or distances (repeat 0))
                               (or namespaces (repeat nil))
                               (range))]
                      (if (and end-at (= end-at idx))
                        (segment
                         (rotate axis angle)
                         (translate axis distance)
                         (add-ns :namespace namespace)
                         (segment list))
                        (branch
                         :from from
                         (rotate axis angle)
                         (translate axis distance)
                         (add-ns :namespace namespace)
                         (segment list))))))))

(defmacro defmodel [name & path*]
  (let [[opts path*] (parse-path path*)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (path* (assoc default-state :name ~(str name)) ~path*)))))





(defn path-models [path*]
  (into {}
        (for [[model-name model] (-> path* meta :models)]
          [model-name
           [(m/union)
            (with-meta
              (m/polygon
               (for [seg model
                     tf (->> seg meta :all-transforms
                             (map u/translation-vector)
                             (map (partial take 2)))]
                 tf))
              (-> model first meta))]])))

(defn path-points [path* select-fn]
  (vec (for [[_ model] (-> path* meta :models)
             seg model
             tf (->> seg meta :all-transforms
                     (map u/translation-vector)
                     (map select-fn))]
         tf)))

(defn ->model-tmp [models path-spec transforms name]
  (let [segs (apply concat (vals models))
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
       :name (or name "default")
       :segment-groups (group-by #(get (meta %) :name :unnamed) segs)
       :models models
       :path-spec path-spec})))

(defmacro poly [& path*]
  (let [[opts path*] (parse-path path*)]
    `(let [p# (path* (assoc default-state :name ~(str name)) ~path*)
          m# (meta p#)]
      (-> p#
          (path-models)
          (->model-tmp (:path-spec m#) (:transforms m#) ~(str name))))))

(defmacro points [& path*]
  (let [[opts path*] (parse-path path*)
        axes (or (:axes opts) [:x :y])
        sym (gensym "tv-")
        clauses (for [axis axes]
                  (case axis
                    :x `(nth ~sym 0)
                    :y `(nth ~sym 1)
                    :z `(nth ~sym 2)))]
    `(let [p# (path* (assoc default-state :name ~(str name)) ~path*)
           m# (meta p#)]
       (path-points p# (fn [~sym]
                         (vector ~@clauses))))))

(defmacro defpoly [name & path*]
  (let [[opts path*] (parse-path path*)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (let [p# (path* (assoc default-state :name ~(str name)) ~path*)
               m# (meta p#)]
           (-> p#
               (path-models)
               (->model-tmp (:path-spec m#) (:transforms m#) ~(str name))))))))

(defn nearest-transform [model d axis]
  )
