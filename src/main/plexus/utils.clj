(ns plexus.utils
  (:require
   [plexus.transforms :as tf]
   [clojure.core.matrix :as mat]
   [clj-manifold3d.core :as man]
   [scad-clj.model :as m]
   [malli.core :as ma]))

#_(mat/set-current-implementation :vectorz)

(def pi Math/PI)

(defn half [x] (/ x 2))

(def registry
  (merge
   (ma/class-schemas)
   (ma/comparator-schemas)
   (ma/base-schemas)
   (ma/predicate-schemas)
   {:neg-int (ma/-simple-schema {:type :neg-int, :pred neg-int?})
    :pos-int (ma/-simple-schema {:type :pos-int, :pred pos-int?})
    :transform (ma/-simple-schema {:type :transform :pred tf/transform?})}))

(defn bAc->a
  "side,angle,side -> side via. law of cosines."
  [b A c]
  (Math/sqrt (- (+ (Math/pow b 2)
                   (Math/pow c 2))
                (* 2 b c (Math/cos A)))))

(defn curve [tf curve-radius curve-angle]
  (let [d (bAc->a curve-radius curve-angle curve-radius)
        r (- (/ Math/PI 2) (/ (- Math/PI curve-angle) 2))]
    (-> tf
        (tf/yaw r)
        (tf/go-forward d)
        (tf/yaw (- curve-angle r)))))

(defn distance [a b]
  (Math/cbrt
   (apply + (mat/mul (mat/sub a b)
                     (mat/sub a b)))))

#_(defn ->inverse-scad-transform
  ([m] (->inverse-scad-transform tf/identity-tf m))
  ([m1 m2]
   (let [translation (mat/sub (tf/translation-vector m2) (tf/translation-vector m1))
         [angle ortho] (tf/rotation-axis-and-angle (tf/rx m1) (tf/rx m2) [0 0 1])
         c (tf/rotate m1 ortho angle)
         [angle-2 ortho-2] (tf/rotation-axis-and-angle (tf/ry m2) (tf/ry c) (tf/rx c))]
     (fn [seg]
       (->> seg
            (m/translate translation)
            (m/rotatev angle ortho)
            (m/rotatev (- angle-2) ortho-2))))))

(defn extrude-rotate [{:keys [angle elevation step-fn model curve-radius face-number] :or {elevation 0} :as args} block]
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

(defn n-faces [v not-found]
  (-> v meta (:n-faces not-found)))

(defn stich-layers [vertices bot top n]
  (assert (= (count top) n))
  (assert (= (count bot) n))
  (loop [i 0
         ret []]
    (if (= i n)
      ret
      (let [prev (if (zero? i) (dec n) (dec i))
            next (if (= (inc i) n) 0 (inc i))]
        (recur (inc i)
               (conj ret
                     [(nth top i)
                      (nth bot i)
                      (nth bot next)]
                     [(nth top i)
                      (nth bot next)
                      (nth top next)]))))))

(defn iso-hull [polys]
  (let [first-poly (first polys)
        poly-res (count first-poly)
        n-indices (* (count polys) poly-res)
        indices (vec (range n-indices))
        vertices (into [] cat polys)
        bottom-face (take poly-res indices)
        top-face (subvec indices (- n-indices poly-res) n-indices)
        iter (mapv vec (partition poly-res indices))
        side-faces (for [[top bot] (partition 2 1 iter)
                         face (stich-layers vertices bot top poly-res)]
                     face)]
    (println "bottom" (count bottom-face))
    (println "top" (count top-face))
    (man/polyhedron vertices
                    (list* bottom-face
                           (rseq top-face)
                           side-faces))))

(defn iso-hull-segments [segs]
  (iso-hull (map (fn [seg]
                     (let [m (meta seg)
                           tf (:start-transform m)
                           profile (:profile m)
                           pts (:points (second profile))]
                       (for [[x y] pts]
                         (-> tf
                             (tf/go-forward x :x)
                             (tf/go-forward y :y)
                             (tf/translation-vector)))))
                 segs)))
