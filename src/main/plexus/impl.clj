(ns plexus.impl
  (:import [manifold3d.glm MatrixTransforms DoubleVec3])
  (:require
   [plexus.utils :as u]
   [malli.core :as ma]
   [clj-manifold3d.core :as m]
   [plexus.triangles :as triangles]
   [malli.core :as ma]
   [plexus.transforms :as tf]))

(defrecord Extrusion [result-forms frames state angle-scalar forms transforms models])

(defrecord Frame [frame-transform segment-transform segments])

(defrecord Segment [start-transform end-transform all-transforms cross-section manifold is-transformed])

(defrecord Model [frame-transform manifold is-transformed])

(defn frame? [x]
  (instance? Frame x))

(defn model? [x]
  (instance? Model x))

;; Segments are relative to frames
;; You can transform frames in result expresions.

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
              :angle-scalar 57.29578}
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
         :models {}})
       (case (:op form)

         :plexus.impl/frame
         (let [frame-id (:name form)
               default-frame-id (:default-frame state)
               default-frame (default-frame-id frames)
               frame-transform (:segment-transform default-frame)
               frame (map->Frame (merge (if (contains? frames frame-id)
                                          (get frames frame-id)
                                          (assoc default-frame :segments []))
                                        (select-keys form [:name :cross-section :curve-radius])
                                        {:frame-transform frame-transform
                                         :segment-transform (m/frame 1)}))]
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
                                    frame (merge frame props)
                                    is-gap (or (true? gap)
                                               (and (sequential? gap) (contains? (set gap) frame-id)))
                                    cross-section (and (not is-gap) (:cross-section frame))]
                                (-> frame
                                    (assoc :segment-transform end-transform)
                                    (update :segments
                                            conj
                                            (Segment. start-transform end-transform all-transforms cross-section
                                                      (when cross-section
                                                        (m/transform (m/extrude cross-section length) start-transform))
                                                      true)))))))
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
                                                             (remove nil? loft-segments))))
                                                          true))))))))
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
                           frame (merge frame props)
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
                                          (Segment. start-transform end-transform all-tfs cross-section manifold true))))))
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
                                          (conj prev-segments (Segment. start-transform end-transform [] nil manifold true))))))))
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
                      (update :transforms #(into (or % {}) (:transforms branch-ret))))
                  forms
                  (reduce-kv
                   (fn [ret frame-id branch-frame]
                     (if (contains? ret frame-id)
                       (assoc ret
                              frame-id
                              (assoc (get ret frame-id) :segments (:segments branch-frame)))
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
           (recur (update state :transforms assoc transform-name (:end-transform frame))
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
                              (assoc frame :end-transform replace))))
                   frames
                   apply-to)
                  result-forms))

         (throw (Exception. (str "No matching clause for form: " form))))))))

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
          tr (#(vector (.x %) (.y %) (.z %)) (.getColumn tf 3))
          manifold (:manifold model)]
      (Model. tf (m/transform manifold tf) true))))

(defn to-manifold [x]
  (cond (frame? x) (-> x to-model project-model :manifold)
        (model? x) (-> x project-model :manifold)
        (m/manifold? x) x
        :else (throw (IllegalArgumentException. (str "Argument must be Frame, Model or Manifold. Recieved: " (type x))))))

(defn extrude [forms]
  (let [result (extrude* (normalize-segment forms))
        frames (:frames result)
        angle-scalar (:angle-scalar result)
        result-forms (:result-forms result)
        unioned-frames (dissoc frames ::default-frame)
        main-model-name (:name (first result-forms))]
    (-> result
        (assoc :main-model main-model-name)
        (assoc :models
               (persistent!
                (reduce-kv
                 (fn [ret k v]
                   (if-let [manifold (to-manifold v)]
                     (assoc! ret k manifold)
                     ret))
                 (transient {})
                 (reduce (fn [result-frames result-form]
                           (assoc result-frames
                                  (:name result-form)
                                  ((fn eval-result [expr]
                                     (if (keyword? expr)
                                       (if-let [frame_ (get result-frames expr)]
                                         frame_
                                         (throw (Exception. (str "Frame is not defined: " expr))))
                                       (if (map? expr)
                                         (if (= (:op expr) :plexus.impl/hull)
                                           (m/hull (map eval-result (let [forms (:plexus.impl/list expr)]
                                                                      (if (sequential? (ffirst forms))
                                                                        (normalize-segment forms)
                                                                        forms))))
                                           (let [ret (eval-result (first (or (:to expr) (:plexus.impl/list expr))))]
                                             (case (:op expr)
                                               :plexus.impl/mirror
                                               (cond (frame? ret) (update (to-model ret) :manifold m/mirror (:normal expr))
                                                     (model? ret) (update ret :manifold m/mirror (:normal expr))
                                                     (m/manifold? ret) (m/mirror ret (:normal expr)))

                                               :plexus.impl/translate
                                               (let [tr [(or (:x expr) 0)
                                                         (or (:y expr) 0)
                                                         (or (:z expr) 0)]]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/translate tr)

                                                       (m/manifold? ret) (m/translate ret tr)))

                                               :plexus.impl/rotate
                                               (let [rot [(* angle-scalar (or (:x expr) 0))
                                                          (* angle-scalar (or (:y expr) 0))
                                                          (* angle-scalar (or (:z expr) 0))]]
                                                 (cond (or (frame? ret) (model? ret)) (update ret :frame-transform m/rotate
                                                                                              [(or (:x expr) 0)
                                                                                               (or (:y expr) 0)
                                                                                               (or (:z expr) 0)]                                    )

                                                       (m/manifold? ret) (m/rotate ret rot)))
                                               (throw (Exception. (str "Unkown expr: " expr))))))
                                         ((case (first expr)
                                            :plexus.impl/difference m/difference
                                            :plexus.impl/union m/union
                                            :plexus.impl/intersection m/intersection
                                            (throw (Exception. (str "Unknown expr: " (vec expr) " ( " (type expr) " )"))))
                                          (map to-manifold
                                               (map eval-result (if (and (sequential? (next expr))
                                                                         (sequential? (first (next expr)))
                                                                         (= (count (next expr)) 1))
                                                                  (second expr)
                                                                  (next expr))))))))
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
                                (with-meta x (select-keys seg meta-props)))
                              identity)]
              tf (->> tfs
                      (map tf/translation-vector)
                      (map select-fn)
                      (map meta-fn))]
          tf))))
