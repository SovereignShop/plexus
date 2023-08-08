(ns plexus.impl
  (:import [manifold3d.glm MatrixTransforms DoubleVec3])
  (:require
   [plexus.utils :as u]
   [malli.core :as ma]
   [clj-manifold3d.core :as m]
   [plexus.triangles :as triangles]
   [plexus.transforms :as tf]))

(defrecord Extrusion [result-forms frames state angle-scalar forms transforms models])

(defrecord Frame [frame-transform segment-transform segments])

(defrecord Segment [start-transform end-transform all-transforms cross-section manifold is-transformed])

(defrecord Model [frame-transform manifold is-transformed])

(defn frame? [x]
  (instance? plexus.impl.Frame x))

(defn model? [x]
  (instance? plexus.impl.Model x))

(defn extrusion? [x]
  (instance? plexus.impl.Extrusion x))

(defn to-model [frame]
  (let [manifolds (remove nil? (map :manifold (:segments frame)))
        manifold (if (pos? (count manifolds))
                   (m/union manifolds)
                   (m/manifold))]
    (Model. (:frame-transform frame) manifold false)))

(defn project-model [model]
  (if (:is-transformed model)
    model
    (let [tf (:frame-transform model)
          manifold (:manifold model)]
      (Model. tf (m/transform manifold tf) true))))

(defn unproject-model [model]
  (if (:is-transformed model)
    (let [tf (:frame-transform model)
          manifold (:manifold model)]
      (Model. tf (m/transform manifold (m/invert-frame tf)) true))
    model))

(defn to-manifold [x]
  (cond (frame? x) (-> x to-model project-model :manifold)
        (model? x) (-> x project-model :manifold)
        (m/manifold? x) x
        (extrusion? x) (get (:models x) (:main-model x))
        :else (throw (IllegalArgumentException. (str "Argument must be Frame, Model or Manifold. Recieved: " (type x))))))

(defn parse-hull [form]
  (if (= (fnext form) :to)
    (let [[op _ to & args] form]
      {:op op :to to ::list args})
    (if (map? (second form))
      (if (:op (second form))
        {:op (first form) ::list (next form)}
        (assoc (second form) :op (first form) ::list (nnext form)))
      {:op (first form) ::list (next form)})))

(defn parse-args
  ([form]
   (parse-args form nil))
  ([form schema]
   (let [op (first form)]
     (case op
       (::union ::difference ::intersection)
       {:op op ::list (next form)}
       (::hull ::loft) (parse-hull form)
       (if (map? (second form))
         (let [op (first form)
               args (nnext form)
               ret (cond-> (assoc (second form) :op op)
                     args (assoc ::list args))]
           (ma/coerce schema ret nil {:registry u/registry}))
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
                   ret))))))))))

(defn parse-path [path-spec]
  (loop [[x & xs] path-spec
         args {}]
    (cond (nil? x)
          [args nil]

          (keyword? x)
          (recur (next xs) (assoc args x (first xs)))

          :else
          [args (vec (cons x xs))])))

(defn form? [x])

(defn normalize-segment [segment]
  (if (sequential? segment)
    (into []
          (comp (mapcat normalize-segment)
                (remove nil?))
          segment)
    [segment]))

(defn ->keyword [namespace name*]
  (keyword (name namespace) (name name*)))

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

(defn extrude*
  ([forms]
   (extrude* {:current-frame-ids #{}
              :default-frame ::default-frame
              :forms forms
              :angle-scalar 57.29578
              :models {}}
             forms
             {::default-frame {:frame-transform (tf/transform)
                               :segment-transform (tf/transform)
                               :segments []}}))
  ([state forms frames]
   (loop [{:keys [current-frame-ids angle-scalar] :as state} state
          [form & forms] forms
          frames frames
          result-forms []]
     (if (nil? form)
       (map->Extrusion
        {:result-forms result-forms
         :frames frames
         :state state
         :angle-scalar angle-scalar
         :forms (:forms state)
         :transforms (:transforms state)
         :models (:models state)})
       (case (:op form)

         :plexus.impl/frame
         (let [frame-id (:name form)
               default-frame-id (:default-frame state)
               _ default-frame-id
               default-frame (get frames default-frame-id)
               default-frame-transform (m/compose-frames
                                        (:frame-transform default-frame)
                                        (:segment-transform default-frame))
               frame (with-meta
                       (map->Frame (merge (if-let [frame (get frames frame-id)]
                                            (assoc frame
                                                   :segment-transform
                                                   (m/compose-frames
                                                    (m/invert-frame (:frame-transform frame))
                                                    default-frame-transform))
                                            (assoc default-frame
                                                   :segments []
                                                   :segment-transform (m/frame 1)
                                                   :frame-transform default-frame-transform))
                                          (select-keys form [:name :cross-section :curve-radius])))
                       (or (:meta form) {}))]
           (recur (-> state
                      (assoc :default-frame frame-id)
                      (update :current-frame-ids conj frame-id))
                  forms
                  (assoc frames frame-id frame)
                  result-forms))

         :plexus.impl/forward
         (let [{:keys [length x y z z to n-steps transform-step-fn twist props gap center branch?]
                :or {transform-step-fn (fn [tf i] tf)}} form
               axis (cond x :x y :y :else :z)
               length (or length x y z)
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (let [start-transform (cond-> (:segment-transform frame)
                                                      center (m/translate [0 0 (- (/ length 2))])
                                                      (= axis :x) (tf/rotate :x (/ Math/PI 2))
                                                      (= axis :y) (tf/rotate :y (- (/ Math/PI 2))))
                                    end-transform (cond-> (:segment-transform frame)
                                                    true (tf/go-forward (cond-> length center (/ 2)) axis))
                                    step-length (if n-steps (/ length n-steps) length)
                                    all-transforms (conj (vec (for [step (range (quot length step-length))]
                                                                (-> (tf/go-forward start-transform (* step step-length) axis)
                                                                    (transform-step-fn step))))
                                                         end-transform)
                                    frame (vary-meta frame merge props)
                                    is-gap (or (true? gap)
                                               (and (sequential? gap) (contains? (set gap) frame-id)))
                                    cross-section (and (not is-gap) (:cross-section frame))]
                                (-> frame
                                    (assoc :segment-transform end-transform)
                                    (update :segments
                                            conj
                                            (with-meta
                                              (Segment. start-transform end-transform all-transforms cross-section
                                                        (when cross-section
                                                          (m/transform (m/extrude cross-section length) start-transform))
                                                        true)
                                              (meta frame))))))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/set
         (let [{:keys [to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (update frames frame-id merge (dissoc form :to :op))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/set-meta
         (let [{:keys [to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames frame-id (vary-meta frame merge (dissoc form :to :op)))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/offset
         (let [{:keys [delta to join-type simplify]
                :or {join-type :square}} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (cond-> frame
                                true (update :cross-section m/offset delta join-type)
                                simplify (update :cross-section m/simplify simplify)))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/loft
         (let [{:keys [to]} form
               loft-forms (normalize-segment (:plexus.impl/list form))
               loft-frames (or to current-frame-ids)
               ret (extrude* state loft-forms frames)]
           (recur (:state ret)
                  forms
                  (reduce
                   (fn [ret-frames frame-id]
                     (let [before-frame (get frames frame-id)
                           ret-frame (get ret-frames frame-id)]
                       (assoc ret-frames
                              frame-id
                              (update ret-frame
                                      :segments
                                      (fn [segments]
                                        (let [n-segments-before-loft (count (:segments before-frame))
                                              loft-segments (subvec segments n-segments-before-loft)
                                              prev-segments (subvec segments 0 n-segments-before-loft)
                                              first-segment (first loft-segments)
                                              last-segment (peek loft-segments)
                                              start-tf (:start-transform (first loft-segments))]
                                          (conj prev-segments
                                                (with-meta
                                                  (Segment. start-tf (:end-transform (peek loft-segments)) [] nil
                                                            (m/loft
                                                             (cons
                                                              {:frame (-> first-segment :all-transforms (nth 0))
                                                               :cross-section (-> first-segment :cross-section)}
                                                              (mapcat
                                                               (fn [{:keys [cross-section all-transforms]}]
                                                                 (for [tf (if (> (count all-transforms) 1)
                                                                            (next all-transforms)
                                                                            all-transforms)]
                                                                   {:cross-section cross-section
                                                                    :frame tf}))
                                                               (remove nil? (filter :cross-section loft-segments)))))
                                                            true)
                                                  (meta ret-frame)))))))))
                   (:frames ret)
                   loft-frames)
                  (into result-forms (:result-forms form))))

         (:plexus.impl/left :plexus.impl/right :plexus.impl/up :plexus.impl/down)
         (let [{:keys [curve-radius angle to op transform-step-fn props gap cs]
                :or {transform-step-fn (fn [tf angle] tf)
                     cs 20}} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)
                           curve-radius (or curve-radius
                                            (:curve-radius frame)
                                            (throw (Exception. "No :curve-radius defined for frame or segment.")))
                           start-transform (:segment-transform frame)
                           d (triangles/bAc->a curve-radius angle curve-radius)
                           r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
                           sign (case op (:plexus.impl/left :plexus.impl/up) - +)
                           f (case op (:plexus.impl/up :plexus.impl/down) tf/pitch tf/yaw)
                           end-transform (-> start-transform
                                             (f (sign r))
                                             (tf/go-forward d)
                                             (f (sign (- angle r))))
                           all-tfs (if gap
                                     []
                                     (all-transforms start-transform
                                                     (fn [tf r d a]
                                                       (-> tf
                                                           (f (sign r))
                                                           (tf/go-forward d)
                                                           (f (sign (- a r)))))
                                                     curve-radius
                                                     angle
                                                     cs
                                                     transform-step-fn))
                           frame (vary-meta frame merge props)
                           is-gap (or (true? gap)
                                      (and (sequential? gap) (contains? (set gap) frame-id)))
                           cross-section (and (not is-gap) (:cross-section frame))
                           manifold (when cross-section
                                      (-> cross-section
                                          (m/rotate 180)
                                          (cond-> (= op :plexus.impl/up) (m/rotate -90)
                                                  (= op :plexus.impl/down) (m/rotate 90)
                                                  (= op :plexus.impl/right) (m/rotate 180))
                                          (m/translate [curve-radius 0])
                                          (m/revolve cs (* angle angle-scalar))
                                          (m/translate [(- curve-radius) 0 0])
                                          (m/rotate [90 0 0])
                                          (m/rotate [0 0 (case op
                                                           :plexus.impl/right 180
                                                           :plexus.impl/up -90
                                                           :plexus.impl/down 90
                                                           0)])
                                          (m/transform start-transform)))]
                       (assoc frames
                              frame-id
                              (-> frame
                                  (assoc :segment-transform end-transform)
                                  (update :segments
                                          conj
                                          (with-meta
                                            (Segment. start-transform end-transform all-tfs cross-section manifold true)
                                            (meta frame)))))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/translate
         (let [{:keys [x y z to global?]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (if global?
                                (update frame :segment-transform #(MatrixTransforms/SetTranslation
                                                                   %1
                                                                   (DoubleVec3. (or x 0) (or y 0) (or z 0))))
                                (update frame :segment-transform m/translate [(or x 0) (or y 0) (or z 0)])))))
                   frames
                   apply-to)
                  result-forms))


         :plexus.impl/rotate
         (let [{:keys [x y z to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (update frame :segment-transform m/rotate [(* 1 (or x 0))
                                                                         (* 1 (or y 0))
                                                                         (* 1 (or z 0))]))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/segment
         (let [segment-forms (normalize-segment (:plexus.impl/list form))]
           (recur state
                  (concat segment-forms forms)
                  frames
                  result-forms))

         :plexus.impl/hull
         (let [{:keys [to]} form
               hull-forms (normalize-segment (:plexus.impl/list form))
               hull-frames (or to current-frame-ids)
               ret (extrude* state hull-forms frames)]
           (recur (:state ret)
                  forms
                  (reduce
                   (fn [ret-frames frame-id]
                     (let [before-frame (get frames frame-id)
                           ret-frame (get ret-frames frame-id)]
                       (assoc ret-frames
                              frame-id
                              (update ret-frame
                                      :segments
                                      (fn [segments]
                                        (let [n-segments-before-hull (count (:segments before-frame))
                                              hull-segments (subvec segments n-segments-before-hull)
                                              prev-segments (subvec segments 0 n-segments-before-hull)
                                              start-transform (:start-transform (first hull-segments))
                                              end-transform (:end-transform (peek hull-segments))
                                              manifold (apply m/hull (remove nil? (map :manifold hull-segments)))]
                                          (conj prev-segments (with-meta
                                                                (Segment. start-transform end-transform [] nil manifold true)
                                                                (meta ret-frame)))))))))
                   (:frames ret)
                   hull-frames)
                  (into result-forms (:result-forms ret))))

         :plexus.impl/branch
         (let [{:keys [from with]} form
               with-frames (or with current-frame-ids)
               branch-ret (extrude* (assoc state
                                           :default-frame from
                                           :current-frame-ids with-frames)
                                    (normalize-segment (:plexus.impl/list form))
                                    frames
                                    #_(select-keys frames (conj with-frames from)))]
           (recur (-> state
                      (update :transforms #(into (or % {}) (:transforms branch-ret)))
                      (update :models merge (:models branch-ret)))
                  forms
                  (reduce-kv
                   (fn [ret frame-id branch-frame]
                     (if-let [frame (get ret frame-id)]
                       (assoc ret
                              frame-id
                              (assoc frame
                                     :segments (:segments branch-frame)))
                       (assoc ret frame-id branch-frame)))
                   frames
                   (:frames branch-ret))
                  (into result-forms (:result-forms branch-ret))))


         :plexus.impl/result
         (recur state forms frames (conj result-forms form))


         :plexus.impl/add-ns
         (let [{:keys [namespace to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (update frame
                                      :namespace
                                      #(keyword
                                        (if %1
                                          (str (name %1) "." (name namespace))
                                          namespace))))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/save-transform
         (let [{:keys [frame to name]} form
               frame (get frames frame)
               namespace (:namespace frame)
               transform-name (cond->> name
                                namespace (->keyword namespace))]
           (recur (update state :transforms assoc transform-name
                          (m/compose-frames
                           (:frame-transform frame)
                           (:segment-transform frame)))
                  forms
                  frames
                  result-forms))

         :plexus.impl/transform
         (let [{:keys [replace to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (assoc frame
                                     :segment-transform
                                     (m/compose-frames
                                      (m/invert-frame (:frame-transform frame))
                                      replace)))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/insert
         (let [{:keys [extrusion models at ns end-frame]} form
               default-frame-id (:default-frame state)
               at (or at default-frame-id)
               base-frame (get frames at)
               base-frame-transform (m/compose-frames
                                     (:frame-transform base-frame)
                                     (:segment-transform base-frame))
               extrusion-models (:models extrusion)
               insert-end-frame (if end-frame
                                  (-> extrusion :frames end-frame)
                                  base-frame)
               end-transform (m/compose-frames
                              (:frame-transform insert-end-frame)
                              (:segment-transform insert-end-frame))
               current-models (:models state)]
           (recur (update state :models into
                          (map (fn [model-id]
                                 (let [model (m/transform (get extrusion-models model-id)
                                                          base-frame-transform)
                                       full-model-id (if ns
                                                       (keyword (name ns) (name model-id))
                                                       model-id)]
                                   [full-model-id
                                    (if-let [current-model (get current-models full-model-id)]
                                      (m/union current-model model)
                                      model)]))
                               (or models [(:main-model extrusion)])))
                  forms
                  (if end-frame
                    (reduce
                     (fn [frames frame-id]
                       (let [frame (get frames frame-id)]
                         (assoc frames
                                frame-id
                                (assoc frame
                                       :segment-transform
                                       (m/compose-frames
                                        end-transform
                                        (:segment-transform frame))))))
                     frames
                     current-frame-ids)
                    frames)
                  result-forms))

         (throw (Exception. (str "No matching clause for form: " form))))))))

(defn extrude [forms]
  (let [result (extrude* (normalize-segment forms))
        frames (:frames result)
        angle-scalar (:angle-scalar result)
        result-forms (:result-forms result)
        unioned-frames (merge (dissoc frames ::default-frame) (:models result))
        main-model-name (or (:name (first result-forms))
                            (key (first (dissoc frames ::default-frame))))
        model-cache (atom {})
        to-manifold-cached (fn [x]
                             (or (get @model-cache x)
                                   (let [ret (to-manifold x)]
                                     (swap! model-cache assoc x ret)
                                     ret)))]
    (-> result
        (assoc :main-model main-model-name)
        (assoc :models
               (persistent!
                (reduce-kv
                 (fn [ret k v]
                   (if (m/manifold? v)
                     (assoc! ret k v)
                     (if-let [manifold (to-manifold v)]
                       (assoc! ret k manifold)
                       ret)))
                 (transient {})
                 (reduce (fn [result-frames result-form]
                           (assoc result-frames
                                  (:name result-form)
                                  ((fn eval-result [expr]
                                     (cond (keyword? expr)
                                           (if-let [frame_ (get result-frames expr)]
                                             frame_
                                             (throw (Exception. (str "Frame is not defined: " expr))))
                                           (m/manifold? expr)
                                           expr
                                           :else
                                           (let [args (normalize-segment (::list expr))]
                                             (case (:op expr)
                                               :plexus.impl/hull
                                               (m/hull (map to-manifold-cached (map eval-result args)))

                                               :plexus.impl/mirror
                                               (let [ret (eval-result (first args))]
                                                 (cond (frame? ret) (update (to-model ret) :manifold m/mirror (:normal expr))
                                                       (model? ret) (update ret :manifold m/mirror (:normal expr))
                                                       (m/manifold? ret) (m/mirror ret (:normal expr))))

                                               :plexus.impl/translate
                                               (let [ret (eval-result (first args))
                                                     tr [(or (:x expr) 0)
                                                         (or (:y expr) 0)
                                                         (or (:z expr) 0)]]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/translate tr)

                                                       (m/manifold? ret) (m/translate ret tr)))

                                               :plexus.impl/rotate
                                               (let [ret (eval-result (first args))]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/rotate
                                                                                              [(or (:x expr) 0)
                                                                                               (or (:y expr) 0)
                                                                                               (or (:z expr) 0)]                                    )

                                                       (m/manifold? ret) (m/rotate ret [(* angle-scalar (or (:x expr) 0))
                                                                                        (* angle-scalar (or (:y expr) 0))
                                                                                        (* angle-scalar (or (:z expr) 0))])))

                                               :plexus.impl/union
                                               (m/union (map to-manifold-cached (map eval-result args)))

                                               :plexus.impl/difference
                                               (m/difference (map to-manifold-cached (map eval-result args)))

                                               :plexus.impl/intersection
                                               (m/intersection (map to-manifold-cached (map eval-result args)))

                                               :plexus.impl/trim-by-plane
                                               (let [ret (eval-result (first args))
                                                     normal (:normal expr)
                                                     origin-offset (or (:origin-offset expr) 0)]
                                                 (m/trim-by-plane ret normal origin-offset))

                                               (throw (Exception. (str "Unknown expr: " expr " ( " (type expr) " )")))))))
                                   (:expr result-form))))
                         unioned-frames
                         (rseq result-forms))))))))

(defn path-points
  ([extrusion]
   (path-points extrude* identity nil))
  ([extrusion select-fn meta-props]
   (vec (for [[_ frame] (:frames extrusion)
              [i seg] (map-indexed list (:segments frame))
              :let [tfs (if (zero? i)
                          (:all-transforms seg)
                          (next (:all-transforms seg)))
                    meta-fn (if meta-props
                              (fn [x]
                                (with-meta x (select-keys (meta seg) meta-props)))
                              identity)]
              tf (->> tfs
                      (map tf/translation-vector)
                      (map select-fn)
                      (map meta-fn))]
          tf))))
