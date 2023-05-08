(ns plexus.impl
  (:refer-clojure :exclude [set])
  (:require
   [clojure.string :as string]
   [clojure.core.matrix :as mat]
   [clojure.walk :refer [postwalk]]
   #_[scad-clj.scad :as s]
   [plexus.segment :as sg]
   [plexus.transforms :as tf]
   [plexus.triangles :as triangles]
   [plexus.utils :as u]
   [scad-clj.model :as m]
   [camel-snake-kebab.core :as csk]
   [malli.core :as ma]))

(def default-model
  {:curve-radius 7
   :profile nil
   :fn 10
   :order 0
   :mask? false
   :name :default
   :models {}
   :start-transform tf/identity-tf
   :end-transform tf/identity-tf})

(def default-state
  {:models {}
   ;; Saved-transforms
   :transforms {}
   :scope []
   :curve-offset 0
   :index -1
   :default-model default-model})

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
         inverse-tf (u/->inverse-scad-transform start-tf tf/identity-tf)]
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
   (remove nil?
           [(.replaceAll (csk/->snake_case (str namespace_))
                         "\\." "_")
            (when (and (keyword? name_) (namespace name_))
              (.replaceAll (csk/->snake_case (str (namespace name_)))
                           "\\." "_"))
            (csk/->snake_case (name name_))])))

(defn seg [model & args]
  (with-meta model (reduce conj default-model (partition 2 args))))

(defn path? [x]
  (-> x meta :path-spec))

(defn parse-args
  ([form]
   (parse-args form nil))
  ([form schema]
   (loop [[arg & args] (next form)
          kvs (transient {:op (first form)})]
     (if (nil? arg)
       (let [ret (persistent! kvs)]
         (if schema
           (ma/coerce schema ret nil {:registry u/registry})
           ret))
       (if (keyword? arg)
         (recur (next args)
                (assoc! kvs arg (first args)))
         (let [ret (persistent! (assoc! kvs ::list (vec (cons arg args))))]
           (if schema
             (ma/coerce schema ret nil {:registry u/registry})
             ret)))))))

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
               x (tf/set-translation x :x)
               y (tf/set-translation y :y)
               z (tf/set-translation z :z)
               true (mat/to-nested-vectors))
             (cond-> start-transform
               x (tf/go-forward x :x)
               y (tf/go-forward y :y)
               z (tf/go-forward z :z)))]
    (vary-meta segment assoc :start-transform tf :projected false)))

(defn rotate-segment
  [segment {:keys [axis angle x y z] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [{:keys [start-transform]} (meta segment)
        axis (if x :x (if y :y (if z :z axis)))
        angle (or x y z angle)
        seg (vary-meta segment assoc :start-transform (tf/rotate start-transform axis angle) :projected false)]
    seg))

(defn result-tree->model
  [{:keys [models result namespace branch-models] :as state}]
  (let [model-table (merge branch-models models)
        segs (mapcat #(transform-segments % identity true)
                     (vals models))
        frame-segs (when-let [frames (:coordinate-frames state)]
                     (transform-segments frames identity true))
        segments (group-by (comp :name meta) segs)
        f (fn walk
            ([tree]
             (if (keyword? tree)
               (let [m (meta (or (second (tree model-table))
                                 (first (tree model-table))
                                 (throw (Exception. (str "result tree model " tree " not found.")))))]
                 (with-meta
                   (sg/normalise (with-meta (m/call-module (make-module-name namespace tree)) m))
                   (assoc m
                          :start-transform tf/identity-tf
                          :end-transform tf/identity-tf
                          :old-model m)))
               (case (first tree)
                 ::model (walk (second tree))
                 ::translate (let [opts (parse-args tree)
                                   segments (::list opts)]
                               (translate-segment (walk (first segments)) opts))
                 ::rotate (let [opts (parse-args tree)]
                            (rotate-segment (walk (first (::list opts))) opts))
                 (let [segs (sequence (comp (map walk) (map (fn [seg]
                                                              (if-let [old-model (:old-model (meta seg))]
                                                                (sg/project (with-meta (sg/project seg) old-model))
                                                                (sg/project seg)))))
                                      (normalize-segment (next tree)))
                       m (meta (last segs))
                       new-m (assoc default-model :old-model (:old-model m))]
                   (case (first tree)
                     ::hull (with-meta (apply m/hull segs) new-m)
                     ::union (with-meta (apply m/union segs) new-m)
                     ::difference (with-meta (apply m/difference segs) new-m)
                     ::intersection (with-meta (apply m/intersection segs) new-m)))))))
        ret  (cond-> (sg/project (let [x (sg/project (f (:expr result)))]
                                   (with-meta x (or (:old-model (meta x)) (meta x)))))
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

(defn ->model [{:keys [models result path-spec transforms name namespace branch-models coordinate-frames] :as state}]
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
                             (string/join "_" (remove nil?
                                                      [(.replaceAll (csk/->snake_case (str namespace))
                                                                    "\\." "_")
                                                       (when (and (keyword? name) (namespace name))
                                                         (.replaceAll (csk/->snake_case (str (clojure.core/namespace name)))
                                                                      "\\." "_"))
                                                       (csk/->snake_case (clojure.core/name name_))
                                                       (if mask? "mask" "body")
                                                       order])))
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
         :coordinate-frames coordinate-frames
         :namespace namespace
         :transforms transforms
         :name (or name "default")
         :segment-groups (group-by #(get (meta %) :name :unnamed) segs)
         :models models
         :path-spec path-spec}))))

(defn write-modules [])

(defn replace-fn [n x]
  (if (and (map? x)
           (or (:fn x)
               (:r x)
               (:angle x)))
    (assoc x :fn n)
    x))

(defn new-fn [model n]
  (postwalk (partial replace-fn n) model))

(defn extrude*
  ([path-forms] (extrude* nil path-forms))
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

(defn extrude
  [path-forms]
  (let [[opts path_] (parse-path path-forms)]
    (extrude* opts path_)))

(defn model-impl* [ret args]
  (let [last-model (when-let [m (get (:models ret) (:last-model ret))]
                     (meta (peek m)))
        model (into (or (dissoc last-model :profile)
                        (:default-model ret))
                    args)
        model (cond-> model
                (:profile model) (update :profile new-fn (or (:fn args) (:fn model))))
        model-name (:name model)
        existing-model (-> ret :models model-name)]
    (if existing-model
      (update ret :models assoc (:name model)
              (conj (pop existing-model)
                    (let [m (peek existing-model)]
                      (cond-> (vary-meta m merge args)
                        (:profile args) (vary-meta  update :profile new-fn (or (:fn args) (:fn (meta (peek existing-model)))))
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
  [{:keys [models index transforms coordinate-frames] :as state} args]
  (let [from-model (:from args)
        _ (assert (contains? models from-model) (str "Attempting branch from undefined model: " from-model))
        with-models (:with args)
        model (get models from-model)
        model-data (meta (peek model))
        branch-state (cond-> (-> state
                                 (update :models assoc from-model (conj (pop model) (vary-meta (peek model) dissoc :branch)))
                                 (dissoc :result)
                                 (assoc :index -1 :default-model model-data)
                                 (update :scope conj index))
                       with-models (update :models select-keys with-models))
        m (extrude* branch-state (::list args))
        new-m (meta m)]
    (assoc state
           :modules (-> m meta :modules)
           :branch-models (merge (:branch-models state) (:models new-m) (:branch-models new-m))
           :models (assoc models
                          from-model
                          (conj (pop model) (vary-meta (peek model) update :branch conj m)))
           :coordinate-frames (into coordinate-frames (-> new-m :coordinate-frames))
           :transforms (merge transforms (-> m meta :transforms)))))

#_(defmethod path-form ::branch
  [{:keys [models index transforms] :as state} args]
  (let [from-model (:from args)
        with-models (:with args)
        model (get models from-model)
        model-data (meta (peek model))
        branch-state (cond-> (-> state
                                 (update :models assoc from-model (conj (pop model) (vary-meta (peek model) dissoc :branch)))
                                 (assoc :index -1 :default-model model-data)
                                 (update :scope conj index))
                       with-models (update :models select-keys with-models))
        m (extrude* branch-state (::list args))
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

(defn generate-profile [profile state]
  (let [models (:models state)
        lookup-crossection (fn [e]
                             (let [model (e models)
                                   m (meta (peek model))
                                   profile_ (:profile m)
                                   tf (:start-transform m)
                                   rot (tf/get-roll tf)]
                               (->> profile_
                                    (m/rotatec [0 0 rot])
                                    #_(m/translate (subvec (tf/translation-vector tf) 0 2)))))
        walk (fn walk [e]
               (if (keyword? e)
                 (lookup-crossection e)
                 (case (first e)
                   ::intersection (apply m/intersection (map walk (next e)))
                   ::union (apply m/union (map walk (next e)))
                   ::difference (apply m/difference (map walk (next e)))
                   e)))]
    (walk profile)))

(defmethod path-form ::save-transform
  [state args]
  (let [model-name (:frame args)
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
    (update state :models assoc model-name (:profile model-state))))

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
                         new-args (if (:profile args)
                                    (-> args
                                        (update :profile generate-profile state)
                                        (update :profile new-fn (:fn m-meta)))
                                    args)
                         m (f (if (:fn new-args)
                                (conj (pop model) (vary-meta (peek model) update :profile new-fn (:fn new-args)))
                                model)
                              (cond-> (assoc m-meta
                                             :op (:op new-args)
                                             :name name
                                             :start-transform (:end-transform (meta (peek model))))
                                (:name new-args) (assoc :name (:name new-args))
                                (:fn new-args) (update :profile new-fn (:fn new-args))
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
   {:keys [radius length frame-offset to label]
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
        frame (->> (m/union x-arrow y-arrow z-arrow
                            (when label
                              (->> (m/text label))))
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

(defn all-transforms [start-transform tf-fn curve-radius angle steps step-fn]
  (let [step-angle (/ angle steps)]
    (map-indexed (fn [i tf]
                   (step-fn tf i))
                 (take (inc steps)
                       (iterate
                        (fn [transform]
                          (let [d (u/bAc->a curve-radius step-angle curve-radius)
                                r (- (/ Math/PI 2) (/ (- Math/PI step-angle) 2))]
                            (tf-fn transform r d step-angle)))
                        start-transform)))))

(defn extrude-left [profile angle curve-radius n-faces]
  (let [degrees (* angle 57.29578)]
    (binding [m/*fn* n-faces]
      (->> profile
           (m/rotatec [Math/PI 0 0])
           (m/translate [curve-radius 0 0])
           (m/extrude-rotate {:angle degrees})
           (m/translate [(- curve-radius) 0 0])
           (m/rotatec [(/ Math/PI 2) 0 0])))))

(defn extrude-right [profile angle curve-radius n-faces]
  (let [degrees (* angle 57.29578)]
    (binding [m/*fn* n-faces]
      (->> profile
           (m/translate [curve-radius 0 0])
           (m/extrude-rotate {:angle degrees})
           (m/translate [(- curve-radius) 0 0])
           (m/rotatec [(- (/ u/pi 2)) u/pi 0])))))

(defn extrude-up [{:keys [angle elevation step-fn model curve-radius face-number]
                   :or {elevation 0} :as args}
                  block]
  (if (and (not step-fn) (= 0 elevation))
    [block (->> block
                (m/rotatec [0 0 (- (/ Math/PI 2))])
                (m/translate [curve-radius 0])
                (m/extrude-rotate args)
                (m/with-fn face-number))]
    (let [steps m/*fn*
          profiles (for [idx (range 0 (inc steps))]
                   (if step-fn (step-fn idx model) block))]
      [(last profiles)
       (->> (for [[idx profile] (map list (range (inc steps)) profiles)]
              (->> (m/with-fn face-number profile)
                   (m/rotatec [0 0 (- (/ Math/PI 2))])
                   (m/translate [curve-radius 0 0])
                   (m/extrude-rotate {:angle 0.02})
                   (m/rotatec [0 0 (* idx (/ angle steps) 0.01745329)])
                   (m/translate [0 0 (* idx (/ elevation steps))])))
            (partition 2 1)
            (map (fn [[l r]]
                   (m/hull l r)))
            (m/union)
            (m/with-fn face-number))])))

(defn extrude-down [profile angle curve-radius n-faces]
  (binding [m/*fn* n-faces]
    (->> profile
         (m/rotatec [0 0 (/ Math/PI 2)])
         (m/translate [curve-radius 0 0])
         (m/extrude-rotate {:angle angle})
         (m/translate [(- curve-radius) 0 0])
         (m/rotatec [(/ u/pi 2) 0 (- (/ u/pi 2))]))))

(defn extrude-left-polyhedron [profile tfs n-faces]
  (u/iso-hull
   (for [tf tfs]
     (case (first profile)
       :circle (let [r (:r (second profile))]
                 (for [i (range n-faces)]
                   (-> tf
                       (tf/rotate :z (* i (/ (* 2 Math/PI) n-faces)))
                       (tf/go-forward r :x)
                       (tf/translation-vector))))))))

(defn extrude-forward [profile length center twist]
  (cond->> profile
    true  (m/extrude-linear {:height (abs length) :center center :twist twist})
    (neg? length) (m/translate [0 0 length])))

(def-segment-handler ::left
  [ret {:keys [profile start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length curve-offset tangent gap transform-step-fn
                tesselation]
         :or {curve-radius (:curve-radius ctx)
              curve-offset (:curve-offset ctx)
              transform-step-fn (fn [tf angle] tf)
              tesselation :default}} args
        n-faces (or (:fn args) (:fn ctx))
        curve-radius (+ curve-radius curve-offset)
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))


        tfs (if (= gap true)
              []
              (all-transforms start-transform
                              (fn [tf r d a]
                                (-> tf
                                    (tf/yaw (- r))
                                    (tf/go-forward d)
                                    (tf/yaw (- (- a r)))))
                              curve-radius
                              angle
                              n-faces
                              transform-step-fn))

        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (tf/yaw (- r))
               (tf/go-forward d)
               (tf/yaw (- (- angle r))))
        part (case tesselation
               :default (extrude-left profile angle curve-radius n-faces)
               :trivert-polyhedron (extrude-left-polyhedron profile tfs n-faces))]
    (conj ret (with-meta part (assoc ctx
                                     :end-transform tf
                                     :all-transforms tfs
                                     :tangent tangent
                                     :curve-radius curve-radius
                                     :curve-origin (-> start-transform
                                                       (tf/yaw (- (/ Math/PI 2)))
                                                       (tf/go-forward curve-radius)))))))

(def-segment-handler ::right
  [ret {:keys [profile start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length curve-offset gap transform-step-fn]
         :or {curve-radius (:curve-radius ctx)
              curve-offset (:curve-offset ctx)
              transform-step-fn (fn [tf angle] tf)}} args
        n-faces (or (:fn args) (:fn ctx))
        curve-radius  (- curve-radius curve-offset)
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        part (extrude-right profile angle curve-radius n-faces)
        tfs (if (= gap true)
              []
              (all-transforms start-transform
                              (fn [tf r d a]
                                (-> tf
                                    (tf/yaw r)
                                    (tf/go-forward d)
                                    (tf/yaw (- a r))))
                              curve-radius
                              angle
                              n-faces
                              transform-step-fn))
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        end-tf  (-> start-transform
                    (tf/yaw r)
                    (tf/go-forward d)
                    (tf/yaw (- angle r)))]
    (conj ret (with-meta part (assoc ctx
                                     :end-transform end-tf
                                     :all-transforms tfs
                                     :curve-radius curve-radius
                                     :curve-origin (-> start-transform
                                                       (tf/yaw (/ Math/PI 2))
                                                       (tf/go-forward curve-radius)))))))

(def-segment-handler ::up
  [ret {:keys [profile gap start-transform name] :as ctx} args]
  (let [{:keys [curve-radius angle gap side-length elevation step-fn
                transform-step-fn]
         :or {curve-radius (:curve-radius ctx)
              elevation 0
              gap gap
              transform-step-fn (fn [tf i] tf)}} args
        face-number (or (:fn args) (:fn ctx))
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees (* angle 57.29578)
        [new-profile model]
        (binding [m/*fn* face-number]
          (extrude-up {:angle degrees
                       :face-number face-number
                       :elevation elevation
                       :frame name
                       :curve-radius curve-radius
                       :step-fn step-fn}
                      profile))
        part (binding [m/*fn* face-number]
               (->> model
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ u/pi 2) 0 (/ u/pi 2)])))
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (tf/pitch r)
                                  (tf/go-forward d)
                                  (tf/go-forward elevation :x)
                                  (tf/pitch (- a r))))
                            curve-radius
                            angle
                            face-number
                            transform-step-fn)
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf (-> start-transform
               (tf/pitch r)
               (tf/go-forward d)
               (tf/go-forward elevation :x)
               (tf/pitch (- angle r)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :profile new-profile :all-transforms tfs)))))

(def-segment-handler ::down
  [ret {:keys [profile start-transform] :as ctx} args]
  (let [{:keys [curve-radius angle side-length transform-step-fn]
         :or {curve-radius (:curve-radius ctx)
              transform-step-fn (fn [tf i])}} args
        face-number (or (:fn args) (:fn ctx))
        angle (if (and side-length (not angle))
                (triangles/abc->A side-length curve-radius curve-radius)
                (if (not angle)
                  (/ Math/PI 2)
                  angle))
        degrees  (* angle 57.29578)
        part (extrude-down profile degrees curve-radius face-number)
        tfs (all-transforms start-transform
                            (fn [tf r d a]
                              (-> tf
                                  (tf/pitch (- r))
                                  (tf/go-forward d)
                                  (tf/pitch (- (- a r)))))
                            curve-radius
                            angle
                            face-number
                            transform-step-fn)
        d (u/bAc->a curve-radius angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        tf  (-> start-transform
                (tf/pitch (- r))
                (tf/go-forward d)
                (tf/pitch (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf :all-transforms tfs)))))

(defn forward-impl* [ret {:keys [profile start-transform step-length n-steps] :as ctx} args]
  (let [{:keys [x y length model twist mask center step-length n-steps branch? order tangent
                transform-step-fn]
         :or {step-length step-length
              n-steps n-steps
              transform-step-fn (fn [tf i] tf)}} args
        axis (cond x :x y :y :else :z)
        length (or length x y (:z args))
        step-length (or step-length (if n-steps (/ length n-steps) length))
        profile (if (and (:fn args) (not= (:fn ctx) (:fn args)))
                (new-fn profile (:fn args))
                profile)
        part (m/with-fn (or (:fn args) (:fn ctx))
               (as-> (if model
                       (new-fn model (or (:fn args) (:fn ctx)))
                       (extrude-forward profile length center twist)) m
                 (if mask
                   (m/difference m mask)
                   m)))
        new-start-transform (cond-> start-transform
                              (= axis :x) (tf/rotate :y (/ Math/PI 2))
                              (= axis :y) (tf/rotate :x (- (/ Math/PI 2))))
        tf (cond-> start-transform
             true (tf/go-forward (cond-> length center (/ 2)) axis)
             twist (tf/rotate :z twist))
        all-transforms (conj (vec (for [step (range (quot length step-length))]
                                    (-> (tf/go-forward new-start-transform (* step step-length) axis)
                                        (transform-step-fn step))))
                             tf)]
    (conj ret (with-meta part (cond-> (assoc ctx
                                             :segment-order order
                                             :tangent tangent
                                             :start-transform new-start-transform
                                             :all-transforms all-transforms)
                                (not branch?) (assoc :end-transform tf)
                                (:meta args) (merge (:meta args)))))))

(def-segment-handler ::forward
  [ret ctx args]
  (forward-impl* ret ctx args))

(def-segment-handler ::offset
  [ret {:keys [fn profile start-transform] :as ctx} args]
  (let [{:keys [length offset]} args
        new-profile  (m/offset offset
                         (if (and (:fn args) (not= (:fn ctx) (:fn args)))
                           (new-fn profile (or (:fn args) (:fn ctx)))
                           profile))
        part (m/with-fn fn
               (cond->> new-profile
                 length (m/extrude-linear {:height length :center false})))
        tf (cond-> start-transform
             length (tf/go-forward length))]
    (if length
      (conj ret (with-meta part (assoc ctx :end-transform tf :profile new-profile)))
      (conj (pop ret) (vary-meta (peek ret) assoc :profile new-profile)))))

(def-segment-handler ::minkowski
  [ret {:keys [fn profile start-transform] :as ctx} args]
  (let [minkowski-profile (:profile args)
        new-profile (m/minkowski minkowski-profile profile)]
    (conj (pop ret) (vary-meta (peek ret) assoc :profile new-profile))))

(def-segment-handler ::backward
  [ret {:keys [fn profile start-transform] :as ctx} args]
  (let [{:keys [length model twist mask center]}  args
        part (binding [m/*fn* fn]
               (as-> (if model
                       (new-fn model (or (:fn args) (:fn ctx)))
                       (->> profile
                            (m/extrude-linear {:height length :center center :twist twist})
                            (m/translate [0 0 (- length)]))) m
                 (if mask
                   (m/difference m mask)
                   m)))
        tf (tf/go-backward start-transform (cond-> length center (/ 2)))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(def-segment-handler ::roll
  [ret _ {:keys [angle] :or {angle (/ Math/PI 2)}}]
  (conj (pop ret) (vary-meta (peek ret) assoc :end-transform (tf/roll (:end-transform (meta (peek ret))) angle))))

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
               x (tf/set-translation x :x)
               y (tf/set-translation y :y)
               z (tf/set-translation z :z)
               true (mat/to-nested-vectors))
             (cond-> start-transform
               x (tf/go-forward x :x)
               y (tf/go-forward y :y)
               z (tf/go-forward z :z)))]
    (conj (pop ret) (vary-meta (peek ret) assoc :end-transform tf))))

(defn rotate-impl
  [ret {:keys [start-transform]} {:keys [axis angle x y z] :or {axis [0 0 1] angle (/ Math/PI 2)}}]
  (let [axis (if x :x (if y :y (if z :z axis)))
        angle (or x y z angle)
        seg (vary-meta (peek ret) assoc :end-transform (tf/rotate start-transform axis angle))]
    (conj (pop ret) seg)))

(def-segment-handler ::rotate
  [ret ctx args]
  (rotate-impl ret ctx args))

(def-segment-handler ::transform
  [ret _ {:keys [transform] :or {transform tf/identity-tf}}]
  (let [seg (vary-meta (peek ret) assoc :end-transform transform)]
    (conj (pop ret) seg)))

(def-segment-handler ::spin
  [ret {:keys [fn profile start-transform] :as ctx} args]
  (let [{:keys [angle axis]
         :or {angle (/ Math/PI 2)}} args
        degrees (* angle 57.29578)
        part (binding [m/*fn* fn]
               (->> (m/difference
                     (cond->> profile
                       (= axis :y) (m/rotatec [0 0 (/ Math/PI 2)]))
                     (->> (m/square 1000 1000)
                          (m/translate [-500 0])))
                    (m/extrude-rotate {:angle degrees})))]
    (conj ret (with-meta part (assoc ctx :end-transform start-transform)))))

(def-segment-handler ::arc
  [ret {:keys [fn profile start-transform] :as ctx} args]
  (let [{:keys [curve-radius side-length]
         :or {curve-radius (:curve-radius ctx)
              side-length 10}} args
        angle (triangles/abc->A side-length curve-radius curve-radius)
        degrees (* angle 57.29578)
        r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
        part (binding [m/*fn* fn]
               (->> profile
                    (m/translate [curve-radius 0 0])
                    (m/extrude-rotate {:angle degrees})
                    (m/translate [(- curve-radius) 0 0])
                    (m/rotatec [(/ Math/PI 2) 0 0])))
        d (u/bAc->a curve-radius angle curve-radius)
        tf (-> start-transform
               (tf/yaw (- r))
               (tf/go-forward d)
               (tf/yaw (- (- angle r))))]
    (conj ret (with-meta part (assoc ctx :end-transform tf)))))

(def-segment-handler ::add-ns
  [ret ctx {:keys [namespace]}]
  (if namespace
    (conj (pop ret) (vary-meta (peek ret) update :namespace #(keyword (if %1
                                                                        (str (name %1) "." (name namespace))
                                                                        namespace))))
    ret))

(def-segment-handler ::union
  [ret _ {:keys [profile]}]
  (conj (pop ret) (vary-meta (peek ret) update :profile m/union profile)))

(def-segment-handler ::forward-until
  [ret {:keys [start-transform]} {:keys [x]}]
  (let [rot (tf/rotation-matrix start-transform)
        z (nth rot 2)
        x* (nth rot 1)
        dx (- x x*)
        angle (tf/angle-between x z)]
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
                          (tf/translation-vector target-transform)
                          (tf/translation-vector start-transform))
        v (mat/sub
           (tf/translation-vector target-transform)
           (tf/translation-vector start-transform))
        r0 (tf/rotation-matrix start-transform)
        [angle ortho] (tf/rotation-axis-and-angle (nth r0 2) v [0 0 1])
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
                      (transform-segments hull-segments u/iso-hull-segments false))]
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
               (tf/go-backward length))
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

(defn get-models [args]
  (for [arg args]
    (if (path? arg)
      (-> arg meta :result)
      (m/call-module
       (make-module-name (-> arg second meta :namespace)
                         (first arg))))))

(defmacro defmodel [name & extrude*]
  (let [[opts extrude*] (parse-path extrude*)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (extrude* (assoc default-state
                       :name ~(str name)
                       :namespace ~(str *ns* "." name))
                ~extrude*)))))

(defmacro defmodel2 [& args]
  (let [[opts extrude*] (parse-path extrude*)
        name (:name opts)
        result (:result opts)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (extrude* (assoc default-state
                       :name ~(str name)
                       :namespace ~(str *ns* "." name))
                ~(cons result extrude*))))))


(defn path-models [extrude*]
  (into {}
        (for [[model-name model] (-> extrude* meta :models)]
          [model-name
           [(m/union)
            (with-meta
              (m/polygon
               (for [seg model
                     tf (->> seg meta :all-transforms
                             (map tf/translation-vector)
                             (map (partial take 2)))]
                 tf))
              (-> model first meta))]])))

(defn path-points
  ([extrude*]
   (path-points extrude* identity nil))
  ([extrude* select-fn meta-props]
   (vec (for [[_ model] (-> extrude* meta :models)
              [i seg] (map-indexed list (remove #(= (-> % meta :op) ::model) model))
              :let [m (meta seg)
                    tangent (:tangent m)
                    tfs (if (zero? i)
                          (:all-transforms m)
                          (next (:all-transforms m)))
                    meta-fn (if meta-props
                              (fn [x]
                                (with-meta x (assoc (select-keys m meta-props) :tangent tangent)))
                              (fn [x]
                                (with-meta x {:tangent tangent})))]
              tf (->> tfs
                      (map tf/translation-vector)
                      (map select-fn)
                      (map meta-fn))]
          tf))))

