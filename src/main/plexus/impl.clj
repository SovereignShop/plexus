(ns plexus.impl
  (:require
   [plexus.utils :as u]
   [malli.core :as ma]
   [clj-manifold3d.core :as m]
   [plexus.triangles :as triangles]
   [malli.core :as ma]
   [plexus.transforms :as tf]))

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

(defn normalize-segment [segment]
  (if (sequential? segment)
    (into []
          (comp (mapcat normalize-segment)
                (remove nil?))
          segment)
    [segment]))

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
              :forms forms}
             forms
             {::default-frame {:start-transform (tf/transform)
                               :end-transform (tf/transform)
                               :segments []}}))
  ([state forms frames]
   (loop [{:keys [current-frame-ids] :as state} state
          [form & forms] forms
          frames frames
          result-forms []]
     (if (nil? form)
       {:result-forms result-forms
        :frames frames
        :state state
        :forms (:forms state)}
       (case (:op form)

         :plexus.impl/frame
         (let [frame-id (:name form)

               default-frame-id (:default-frame state)
               frame (merge (if (contains? frames frame-id)
                              (get frames frame-id)
                              (assoc (default-frame-id frames) :segments []))
                            (select-keys form [:name :cross-section]))
               tv (tf/translation-vector (:end-transform frame))]
           (recur (-> state
                      (assoc :default-frame frame-id)
                      (update :current-frame-ids conj frame-id))
                  forms
                  (assoc frames frame-id frame)
                  result-forms))

         :plexus.impl/forward
         (let [{:keys [length z to n-steps transform-step-fn twist props gap]
                :or {transform-step-fn (fn [tf i] tf)}} form
               length (or z length)
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (let [start-transform (:end-transform frame)
                                    end-transform (m/translate start-transform [0 0 length])
                                    step-length (if n-steps (/ length n-steps) length)
                                    all-transforms (conj (vec (for [step (range (quot length step-length))]
                                                                (-> (tf/go-forward start-transform (* step step-length) :z)
                                                                    (transform-step-fn step))))
                                                         end-transform)
                                    frame (merge frame props)]
                                (-> frame
                                    (assoc :end-transform end-transform
                                           :start-transform start-transform)
                                    (update :segments
                                            conj
                                            (assoc frame
                                                   :start-transform start-transform
                                                   :end-transform end-transform
                                                   :all-transforms (if gap [] all-transforms)
                                                   :manifold (when-let [cross-section (and (not gap) (:cross-section frame))]
                                                               (m/transform (m/extrude cross-section length) start-transform)))))))))
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

         (:plexus.impl/left :plexus.impl/right :plexus.impl/up :plexus.impl/down)
         (let [{:keys [curve-radius angle to op transform-step-fn props gap]
                :or {transform-step-fn (fn [tf angle] tf)}} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)
                           start-transform (:end-transform frame)
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
                                                     20
                                                     transform-step-fn))
                           frame (merge frame props)]
                       (assoc frames
                              frame-id
                              (-> frame
                                  (assoc :end-transform end-transform
                                         :start-transform start-transform)
                                  (update :segments
                                          conj
                                          (assoc frame
                                                 :start-transform start-transform
                                                 :end-transform end-transform
                                                 :all-transforms all-tfs
                                                 :manifold
                                                 (when-let [cross-section (and (not gap) (:cross-section frame))]
                                                   (-> cross-section
                                                       (m/rotate 180)
                                                       (cond-> (= op :plexus.impl/up) (m/rotate -90)
                                                               (= op :plexus.impl/down) (m/rotate 90)
                                                               (= op :plexus.impl/right) (m/rotate 180))
                                                       (m/translate [curve-radius 0])
                                                       (m/revolve 20 (* angle 57.29578))
                                                       (m/translate [(- curve-radius) 0 0])
                                                       (m/rotate [(* 57.29578 (/ Math/PI 2)) 0 0])
                                                       (m/rotate [0 0 (case op
                                                                        :plexus.impl/right 180
                                                                        :plexus.impl/up -90
                                                                        :plexus.impl/down 90
                                                                        0)])
                                                       (m/transform start-transform)))))))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/translate
         (let [{:keys [x y z to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (let [frame (get frames frame-id)]
                       (assoc frames
                              frame-id
                              (update frame :end-transform m/translate [(or x 0) (or y 0) (or z 0)]))))
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
                              (update frame :end-transform m/rotate [(or x 0) (or y 0) (or z 0)]))))
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
                                              prev-segments (subvec segments 0 n-segments-before-hull)]
                                          (conj prev-segments
                                                {:start-transform (:start-transform (first hull-segments))
                                                 :end-transform (:end-transform (peek hull-segments))
                                                 :manifold (apply m/hull (map :manifold (remove nil? hull-segments)))})))))))
                   (:frames ret)
                   hull-frames)
                  (into result-forms (:result-forms ret))))

         :plexus.impl/branch
         (let [{:keys [from with]} form
               with-frames (or with current-frame-ids)
               ret (extrude* (assoc state :default-frame from :current-frame-ids with-frames)
                             (normalize-segment (:plexus.impl/list form))
                             frames)]
           (recur state
                  forms
                  (reduce-kv
                   (fn [ret frame-id branch-frame]
                     (let [frame (get ret frame-id)]
                       (if (contains? ret frame-id)
                         (assoc ret
                                frame-id
                                (update frame :segments into (:segments branch-frame)))
                         (assoc frames frame-id branch-frame))))
                   frames
                   (:frames ret))
                  (into result-forms (:result-forms ret))))


         :plexus.impl/result
         (recur state forms frames (conj result-forms form)))))))

(defn extrude [forms]
  (let [result (extrude* (normalize-segment forms))
        frames (:frames result)
        result-forms (:result-forms result)
        unioned-frames (into {}
                             (comp (map (fn [[frame-id frame]]
                                          (let [manifolds (remove nil? (map :manifold (:segments frame)))]
                                            (when (pos? (count manifolds))
                                              [frame-id (apply m/union manifolds)]))))
                                   (remove nil?))
                             (dissoc frames ::default-frame))
        main-frame-name (:name (first result-forms))]
    (update (assoc result :main-frame main-frame-name)
            :frames
            merge
            (reduce (fn [result-frames result-form]
                      (assoc result-frames
                             (:name result-form)
                             ((fn eval-result [expr]
                                (if (keyword? expr)
                                  (get result-frames expr)
                                  (if (map? expr)
                                    (let [manifold (eval-result (first (:plexus.impl/list expr)))]
                                      (case (:op expr)
                                        :plexus.impl/translate (m/translate manifold [(or (:x expr) 0) (or (:y expr) 0) (or (:z expr) 0)])
                                        :plexus.impl/rotate (m/rotate manifold [(or (:x expr) 0) (or (:y expr) 0) (or (:z expr) 0)])))
                                    ((case (first expr)
                                       :plexus.impl/difference m/difference
                                       :plexus.impl/union m/union
                                       :plexus.impl/intersection m/intersection)
                                     (map eval-result (next expr))))))
                              (:expr result-form))))
                    unioned-frames
                    (rseq result-forms)))))

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
