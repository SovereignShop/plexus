(ns plexus.impl2
  (:require
   [clj-manifold3d.core :as m]
   [plexus.triangles :as triangles]
   [plexus.transforms :as tf]))

(defn extrude*
  ([forms]
   (extrude* {:current-frame-ids #{} :default-frame ::default-frame}
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
        :frames frames}
       (case (:op form)

         :plexus.impl/frame
         (let [frame-id (:name form)
               frame (merge (::default-frame frames) (select-keys form [:name :cross-section]))]
           (recur (-> state
                      (assoc :default-frame frame-id)
                      (update :current-frame-ids conj frame-id))
                  forms
                  (assoc frames frame-id frame)
                  result-forms))

         :plexus.impl/forward
         (let [{:keys [length to]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (update frames
                             frame-id
                             (fn [frame]
                               (let [transform (:end-transform frame)
                                     cross-section (:cross-section frame)
                                     manifold (m/transform (m/extrude cross-section length) transform)
                                     end-transform (m/translate transform [0 0 length])]
                                 (-> frame
                                     (assoc :end-transform end-transform)
                                     (update :segments
                                             conj
                                             (assoc frame
                                                    :start-transform transform
                                                    :end-transform end-transform
                                                    :manifold manifold)))))))
                   frames
                   apply-to)
                  result-forms))

         (:plexus.impl/left :plexus.impl/right)
         (let [{:keys [curve-radius angle to op]} form
               apply-to (or to current-frame-ids)]
           (recur state
                  forms
                  (reduce
                   (fn [frames frame-id]
                     (update frames
                             frame-id
                             (fn [frame]
                               (let [start-transform (:end-transform frame)
                                     d (triangles/bAc->a curve-radius angle curve-radius)
                                     r (- (/ Math/PI 2) (/ (- Math/PI angle) 2))
                                     sign (case op :plexus.impl/left - +)
                                     end-transform (-> start-transform
                                                       (tf/yaw (sign r))
                                                       (tf/go-forward d)
                                                       (tf/yaw (sign (- angle r))))
                                     cross-section (:cross-section frame)
                                     manifold (-> cross-section
                                                  (m/translate [curve-radius 0])
                                                  (m/revolve 20 (* angle 57.29578))
                                                  (m/translate [(- curve-radius) 0 0])
                                                  (m/rotate [(* 57.29578 (/ Math/PI 2)) 0 0])
                                                  (cond-> (= op :plexus.impl/right)
                                                    (m/rotate [0 0 180]))
                                                  (m/transform start-transform))]
                                 (-> frame
                                     (assoc :end-transform end-transform)
                                     (update :segments
                                             conj
                                             (assoc frame
                                                    :start-transform start-transform
                                                    :end-transform end-transform
                                                    :manifold manifold)))))))
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
                     (update frames
                             frame-id
                             (fn [frame]
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
                     (update frames
                             frame-id
                             (fn [frame]
                               (update frame :end-transform m/rotate [(or x 0) (or y 0) (or z 0)]))))
                   frames
                   apply-to)
                  result-forms))

         :plexus.impl/branch
         (let [{:keys [from with]} form
               with-frames (if with (select-keys frames with) current-frame-ids)
               ret (extrude* (assoc state :default-frame from :current-frame-ids with-frames)
                             (:plexus.impl/list form)
                             frames)]
           (recur state
                  forms
                  (reduce-kv
                   (fn [ret frame-id branch-frame]
                     (if (contains? ret frame-id)
                       (update ret
                               frame-id
                               (fn [frame]
                                 (update frame :segments into (:segments branch-frame))))
                       (assoc frames frame-id branch-frame)))
                   frames
                   (:frames ret))
                  (into result-forms (:result-forms ret))))


         :plexus.impl/result
         (recur state forms frames (conj result-forms form)))))))

(defn extrude [& forms]
  (let [{:keys [frames result-forms]} (extrude* forms)
        unioned-frames (into {}
                             (map (fn [[frame-id frame]]
                                    [frame-id (apply m/union (map :manifold (:segments frame)))]))
                             (dissoc frames ::default-frame))]
    (reduce (fn [result-frames result-form]
              (assoc result-frames
                     (:name result-form)
                     ((fn eval-result [expr]
                        (if (keyword? expr)
                          (get result-frames expr)
                          (apply
                           (case (first expr)
                             :plexus.impl/difference m/difference
                             :plexus.impl/union m/union
                             :plexus.impl/intersection m/intersection)
                           (map eval-result (next expr)))))
                      (:expr result-form))))
            unioned-frames
            result-forms)))


(def)
