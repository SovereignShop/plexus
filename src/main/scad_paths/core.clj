(ns scad-paths.core
  (:refer-clojure :exclude [set])
  (:require
   [clojure.string :as string]
   [clojure.core.matrix :as mat]
   [clojure.walk :refer [postwalk]]
   [scad-clj.scad :as s]
   [scad-paths.transforms :as tr]
   [scad-paths.segment :as sg]
   [scad-paths.triangles :as triangles]
   [scad-paths.utils :as u]
   [scad-clj.model :as m]
   [camel-snake-kebab.core :as csk]))

(defmulti path-form (fn [_ args] (:op args)))

(derive ::left ::curve)
(derive ::right ::curve)
(derive ::up ::curve)
(derive ::down ::curve)

(derive ::forward ::vector)

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

(defn make-module-name
  [namespace_ name_]
  (string/join
   "_"
   [(.replaceAll (csk/->snake_case (str namespace_))
                 "\\." "_")
    (csk/->snake_case (name name_))]))

(def default-model
  {:curve-radius 7
   :shape nil
   :fn 10
   :order 0
   :mask? false
   :name :default
   :models {}
   :start-transform u/identity-mat
   :end-transform u/identity-mat})

(defn seg [model & args]
  (with-meta model (reduce conj default-model (partition 2 args))))

(defn path? [x]
  (-> x meta :path-spec))

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
          (if (and (sequential? (first segment))
                   (sequential? (ffirst segment)))
            (first segment)
            segment)))

(defn translate-segment [segment {:keys [x y z global?]}]
  (let [{:keys [start-transform] :as m} (meta segment)
        tf (if global?
             (cond-> start-transform
               x (u/set-translation x :x)
               y (u/set-translation y :y)
               z (u/set-translation z :z)
               true (mat/to-nested-vectors))
             (cond-> start-transform
               x (u/go-forward x :x)
               y (u/go-forward y :y)
               z (u/go-forward z :z)))]
    (vary-meta segment assoc :start-transform tf)))

(defn rotate-segment
  [segment {:keys [axis angle x y z] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [{:keys [start-transform]} (meta segment)
        axis (if x :x (if y :y (if z :z axis)))
        angle (or x y z angle)
        seg (vary-meta segment assoc :start-transform (u/rotate start-transform axis angle))]
    seg))

(defn result-tree->model
  [{:keys [models result namespace branch-models] :as state}]
  (let [model-table (merge models branch-models)
        segs (mapcat #(transform-segments % identity true)
                     (vals models))
        frame-segs (when-let [frames (:coordinate-frames state)]
                     (transform-segments frames identity true))
        segments (group-by (comp :name meta) segs)
        f (fn walk
            ([tree]
             (if (keyword? tree)
               (let [m (meta (or (second (tree model-table))
                                 (first (tree model-table))))]
                 (with-meta
                   (sg/normalise (with-meta (m/call-module (make-module-name namespace tree)) m))
                   (assoc m
                          :start-transform u/identity-mat
                          :end-transform u/identity-mat
                          :old-model m)))
               (case (first tree)
                 ::model (walk (second tree))
                 ::translate (let [opts (parse-args tree)
                                   segments (::list opts)]
                               (translate-segment (walk (first segments)) opts))
                 ::rotate (let [opts (parse-args tree)]
                            (rotate-segment (walk (first (::list opts))) opts))
                 (let [segs (sequence (comp (map walk) #_(map sg/project #_(fn [seg]
                                                              (if-let [old-model (:old-model (meta seg))]
                                                                (sg/project (with-meta seg old-model))
                                                                (sg/project seg)))))
                                      (normalize-segment (next tree)))
                       m (meta (last segs))
                       new-m (assoc default-model :old-model (:old-model m))]
                   (case (first tree)
                     ::hull (with-meta (apply m/hull segs) new-m)
                     ::union (with-meta (apply m/union segs) new-m)
                     ::difference (with-meta (apply m/difference segs) new-m)
                     ::intersection (with-meta (apply m/intersection segs) new-m)))))))
        ret  (cond-> #_(sg/project) (let [x #_(sg/project) (f (:expr result))]
                                    (with-meta x (or (:old-model (meta x))  (meta x))))
               frame-segs (m/union frame-segs)
               true (vary-meta assoc :name (:name result)))
        modules (reduce-kv (fn [ret name_ seg]
                             (if (= name_ :default)
                               ret
                               (assoc ret name_ (m/define-module (make-module-name namespace name_) (apply m/union seg)))))
                           {}
                           (assoc segments (:name result) [ret]))]
    (-> state
        (update :modules merge modules)
        (assoc :result (vary-meta ret dissoc :old-model))
        (assoc :segment-groups (group-by #(get (meta %) :name :unnamed) segs))
        (update :models assoc (:name result) [(vary-meta ret dissoc :old-model)]))))

(defn ->model [{:keys [models result path-spec transforms name namespace branch-models] :as state}]
  (if result
    (let [state
          (reduce (fn [state [_ result]]
                    (result-tree->model (assoc state :result result)))
                  state
                  (sort-by (comp - first key) result))
          modules (:modules state)]
      (with-meta (conj (vec (vals (sort-by key modules)))
                       (:result state))
        state))
    (let [segs (mapcat #(transform-segments % identity true)
                       (vals models))
          make-module-name (fn [order mask? name_]
                             (string/join "_" [(.replaceAll (csk/->snake_case (str namespace))
                                                            "\\." "_")
                                               (clojure.core/name (csk/->snake_case name_))
                                               (if mask? "mask" "body")
                                               order]))
          frame-segs (when-let [frames (:coordinate-frames state)]
                       (transform-segments frames identity true))
          segment-groups (->> segs
                              (group-by (juxt
                                         (fn [x]
                                           (let [m (meta x)]
                                             (or (:segment-order m) (:order m))))
                                         (comp :mask? meta)
                                         (comp :name meta))))
          segments (sort-by key segment-groups)
          modules (mapv (fn [[[order mask? name_] seg]]
                          (m/define-module (make-module-name order mask? name_) (apply m/union seg)))
                        segments)]
      (with-meta
        (conj modules
              (cond-> (reduce (fn [ret [[order mask? name_] _]]
                                (if mask?
                                  (m/difference ret (m/call-module (make-module-name order mask? name_)))
                                  (m/union ret (m/call-module (make-module-name order mask? name_)))))
                              (m/union)
                              segments)
                frame-segs (m/union frame-segs)))
        {:segments segments
         :branch-models branch-models
         :namespace namespace
         :transforms transforms
         :name (or name "default")
         :segment-groups (group-by #(get (meta %) :name :unnamed) segs)
         :models models
         :path-spec path-spec}))))

(defn write-modules [])

(defn lookup-transform
  [model name]
  (or (-> model meta :segment-groups name peek meta :end-transform)
      (-> model meta :transforms name)))

(defn lookup-property
  [model name property]
  (-> model meta :segment-groups name peek meta property))

(defn replace-fn [n x]
  (if (and (map? x)
           (or (:fn x)
               (:r x)
               (:angle x)))
    (assoc x :fn n)
    x))

(defn new-fn [model n]
  (postwalk (partial replace-fn n) model))

(def default-state
  {:models {}
   :transforms {}
   :scope []
   :curve-offset 0
   :index -1
   :default-model default-model})

(defn path*
  ([path-forms] (path* nil path-forms))
  ([state path-forms]
   (loop [{:keys [models index] :as state} (merge default-state state)
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
  (let [[opts path_] (parse-path path-forms)]
    (path* opts path_)))

(defn model-impl* [ret args]
  (let [last-model (when-let [m (get (:models ret) (:last-model ret))]
                     (meta (peek m)))
        model (into (or (dissoc last-model :shape)
                        (:default-model ret))
                    args)
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
          (update :models assoc model-name [(with-meta (m/union)
                                              (-> model
                                                  (assoc :curve-offset (:curve-offset ret))))])
          (assoc :last-model model-name)))))

(defmethod path-form ::model
  [ret args]
  (model-impl* ret args))

(defmethod path-form ::branch
  [{:keys [models index transforms] :as state} args]
  (let [from-model (:from args)
        with-models (:with args)
        model (get models from-model)
        model-data (meta (peek model))
        branch-state (cond-> (-> state
                                 (update :models assoc from-model (conj (pop model) (vary-meta (peek model) dissoc :branch)))
                                 (dissoc :result)
                                 (assoc :index -1 :default-model model-data)
                                 (update :scope conj index))
                       with-models (update :models select-keys with-models))
        m (path* branch-state (::list args))
        new-m (meta m)]
    (assoc state
           :modules (-> m meta :modules)
           :branch-models (merge (:branch-models state) (:models new-m) (:branch-models new-m))
           :models (assoc models
                          from-model
                          (conj (pop model) (vary-meta (peek model) update :branch conj m)))
           :transforms (merge transforms (-> m meta :transforms)))))

(defn ->keyword [namespace name*]
  (keyword (name namespace) (name name*)))

(defn generate-shape [shape state]
  (let [models (:models state)
        lookup-crossection (fn [e]
                             (let [model (e models)
                                   m (meta (peek model))
                                   shape_ (:shape m)
                                   tf (:start-transform m)
                                   rot (u/get-roll tf)]
                               (->> shape_
                                    (m/rotatec [0 0 rot])
                                    #_(m/translate (subvec (u/translation-vector tf) 0 2)))))
        walk (fn walk [e]
               (if (keyword? e)
                 (lookup-crossection e)
                 (case (first e)
                   ::intersection (apply m/intersection (map walk (next e)))
                   ::union (apply m/union (map walk (next e)))
                   ::difference (apply m/difference (map walk (next e)))
                   e)))]
    (walk shape)))

(defmethod path-form ::save-transform
  [state args]
  (let [model-name (:model args)
        model (get (:models state) model-name)
        model-state (-> model peek meta)
        namespace (:namespace model-state)
        transform-name (cond->> (:name args)
                         namespace (->keyword namespace))]
    (update state :transforms assoc transform-name (:end-transform model-state))))

(defmethod path-form ::save-model
  [state args]
  (let [model-name (:name args)
        model (get (:models state) model-name)
        model-state (-> model peek meta)]
    (update state :models assoc model-name (:shape model-state))))

(defmethod path-form ::result
  [state args]
  (let [result (:result state)]
    (assoc state :result (assoc result [(count result) (:name args)] args))))

(defn update-models [{:keys [models ignored-models] :as state} {:keys [to gap] :as args} f]
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
                                    (-> args
                                        (update :shape generate-shape state)
                                        (update :shape new-fn (:fn m-meta)))
                                    args)
                         m (f (if (:fn new-args)
                                (conj (pop model) (vary-meta (peek model) update :shape new-fn (:fn new-args)))
                                model)
                              (cond-> (assoc m-meta
                                             :op (:op new-args)
                                             :name name
                                             :start-transform (:end-transform (meta (peek model))))
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
                                                mta)
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
  (conj (pop ret) (vary-meta (peek ret) merge
                             (-> args
                                 (dissoc :op)))))

(defmethod path-form ::show-coordinate-frame
  [state
   {:keys [radius length frame-offset to]
    :or {length 20 radius 1 frame-offset [0 0 0]}
    :as args}]
  (let [models (cond-> (:models state)
                 to (select-keys to))

        arrow (m/cylinder radius length :center false)
        x-arrow (->> arrow
                     (m/rotate [0 (/ Math/PI 2) 0])
                     (m/color [1 0 0]))
        y-arrow (->> arrow
                     (m/rotate [(- (/ Math/PI 2)) 0 0])
                     (m/color [0 1 0]))
        z-arrow (->> arrow
                     (m/color [0 0 1]))
        frame (->> (m/union x-arrow y-arrow z-arrow)
                   (m/translate frame-offset))]
    (reduce (fn [state model]
              (let [m (meta (peek model))
                    {:keys [end-transform]} m]
                (update state
                        :coordinate-frames
                        conj
                        (with-meta frame (assoc m
                                                :mask? false
                                                :type :coordinate-frame
                                                :start-transform end-transform
                                                :end-transform end-transform)))))
            state
            (vals models))))


(defn left-curve-points [curve-radius curve-angle face-number]
  (let [step-size (/ curve-angle face-number)]
    (for [x (range 0 curve-angle step-size)]
      [(- (* curve-radius (Math/cos x)) curve-radius)
       (* curve-radius (Math/sin x))])))

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
  (let [{:keys [curve-radius angle side-length curve-offset tangent]
         :or {curve-radius (:curve-radius ctx)
              curve-offset (:curve-offset ctx)}} args
        n-faces (or (:fn args) (:fn ctx))
        curve-radius (+ curve-radius curve-offset)
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (binding [m/*fn* n-faces]
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
                            (/ n-faces 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw (- r))
               (u/go-forward d)
               (u/yaw (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx
                                     :end-transform tf
                                     :all-transforms tfs
                                     :tangent tangent
                                     :curve-radius curve-radius
                                     :curve-origin (-> start-transform
                                                       (u/yaw (- (/ Math/PI 2)))
                                                       (u/go-forward curve-radius)))))))

(def-segment-handler ::right
  [ret {:keys [shape start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length curve-offset]
         :or {curve-radius (:curve-radius ctx)
              curve-offset (:curve-offset ctx)}} args
        n-faces (or (:fn args) (:fn ctx))
        curve-radius  (- curve-radius curve-offset)
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (binding [m/*fn* n-faces]
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
                            (/ n-faces 2))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (u/yaw r)
               (u/go-forward d)
               (u/yaw (- angle r)))]
    (conj ret (with-meta part (assoc ctx
                                     :end-transform tf
                                     :all-transforms tfs
                                     :curve-radius curve-radius
                                     :curve-origin (-> start-transform
                                                       (u/yaw (/ Math/PI 2))
                                                       (u/go-forward curve-radius)))))))

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

(defn forward-impl* [ret {:keys [fn shape start-transform step-length n-steps] :as ctx} args]
  (let [{:keys [x y length model twist mask center step-length n-steps branch? order tangent]
         :or {step-length step-length
              n-steps n-steps}} args
        axis (cond x :x y :y :else :z)
        length (or length x y (:z args))
        step-length (or step-length (if n-steps (/ length n-steps) length))
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
        new-start-transform (cond-> start-transform
                              (= axis :x) (u/rotate :y (/ Math/PI 2))
                              (= axis :y) (u/rotate :x (- (/ Math/PI 2))))
        tf (-> start-transform
               (u/go-forward (cond-> length center (/ 2)) axis))
        all-transforms (conj (vec (for [step (range (quot length step-length))]
                                    (u/go-forward new-start-transform (* step step-length) axis)))
                             tf)]
    (conj ret (with-meta part (cond-> (assoc ctx
                                             :segment-order order
                                             :tangent tangent
                                             :start-transform new-start-transform
                                             :all-transforms all-transforms)
                                (not branch?) (assoc :end-transform tf))))))

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

(def-segment-handler ::iso-hull
  [ret {:keys [start-transform] :as ctx} {:keys [n-segments] :or {n-segments 2} :as args}]
  (let [hull-segments (into () (take n-segments) (map peek (iterate pop ret)))
        other-forms (nth (iterate pop ret) n-segments)
        new-segment (binding [m/*fn* (or (:fn args) (:fn ctx))]
                      (transform-segments hull-segments u/iso-hull false))]
    (conj other-forms new-segment)))

(def-segment-handler ::import
  [ret ctx {:keys [stl] :as args}]
  (let [model (m/import stl)]
    (conj ret (with-meta model (meta (peek ret))))))

(def-segment-handler ::slice
  [ret ctx {:keys [length] :as args :or {length 0.01}}]
  (let [seg (peek ret)
        m (meta seg)
        tf (-> (:end-transform m)
               (u/go-backward length))
        new-m (assoc m
                     :start-transform tf
                     :end-transform (:end-transform m))
        new-seg (with-meta
                  (->> (m/square 1000 1000)
                       (m/extrude-linear {:height length :center false}))
                  new-m)
        new-seg (with-meta
                  (sg/normalise
                   (with-meta
                     (m/intersection
                      (sg/project new-seg)
                      (sg/project seg))
                     new-m))
                  new-m)]
    (conj ret new-seg)))

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

(defn slice [& args]
  `(::slice ~@args))

(defn get-models [args]
  (for [arg args]
    (if (path? arg)
      (-> arg meta :result)
      (m/call-module
       (make-module-name (-> arg second meta :namespace)
                         (first arg))))))

(defn subtract [a & args]
  (let [all-modules (reduce (fn [ret arg]
                              (if (path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (get-models (cons a args))
        result (apply m/difference all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defn intersect [a & args]
  (let [all-modules (reduce (fn [ret arg]
                              (if (path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (get-models (cons a args))
        result (apply m/intersection all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defn join [a & args]
  (let [all-modules (reduce (fn [ret arg]
                              (if (path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (get-models (cons a args))
        result (apply m/union all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defn iso-hull [& args]
  `(::iso-hull ~@args))

(defn import [& args]
  `(::import ~@args))

(defn show-coordinate-frame [& args]
  `(::show-coordinate-frame ~@args))

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
         (path* (assoc default-state
                       :name ~(str name)
                       :namespace ~(str *ns* "." name))
                ~path*)))))

(defmacro defmodel2 [& args]
  (let [[opts path*] (parse-path path*)
        name (:name opts)
        result (:result opts)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (path* (assoc default-state
                       :name ~(str name)
                       :namespace ~(str *ns* "." name))
                ~(cons result path*))))))


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

(defn path-points
  ([path*]
   (path-points path* identity))
  ([path* select-fn]
   (vec (for [[_ model] (-> path* meta :models)
              seg model
              :let [m (meta seg)
                    tangent (:tangent m)
                    tfs (:all-transforms m)]
              tf (->> tfs
                      (map u/translation-vector)
                      (map select-fn)
                      (map #(with-meta % {:tangent tangent})))]
          tf))))

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
       (with-meta
         (path-points p# (fn [~sym]
                           (vector ~@clauses)))
         m#))))

(defmacro defpoly [name & path*]
  (let [[opts path*] (parse-path path*)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (let [p# (path* (assoc default-state :name ~(str name)) ~path*)
               m# (meta p#)]
           (-> p#
               (path-models)
               (->model-tmp (:path-spec m#) (:transforms m#) ~(str name))))))))
