(ns plexus.utils
  (:require
   [plexus.transforms :as tf]
   [clojure.core.matrix :as mat]
   [clj-manifold3d.core :as man]
   [malli.core :as ma]
   [clj-manifold3d.core :refer [cross-section?]]))

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
    :cross-section (ma/-simple-schema {:type :cross-section :pred cross-section?})
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
    (man/polyhedron vertices
                    (list* bottom-face
                           (rseq top-face)
                           side-faces))))

#_(defn iso-hull-segments [segs]
  (iso-hull (map (fn [seg]
                     (let [tf (:start-transform seg)
                           section (:cross-section m)
                           polys (man/to-polygons section)]
                       (for [poly polys]
                         (-> tf
                             (tf/go-forward (.x vert) :x)
                             (tf/go-forward (.y vert) :y)
                             (tf/translation-vector)))))
                 segs)))


#_(defn iso-hull-segments [segs]
  (for [polys (apply map vector (map (fn [seg]
                                       (let [tf (:start-transform seg)
                                             section (:cross-section m)
                                             polys (man/to-polygons section)]
                                         (for [poly polys]
                                           (for [vert poly]
                                             (-> tf
                                                 (tf/go-forward (.x vert) :x)
                                                 (tf/go-forward (.y vert) :y)
                                                 (tf/translation-vector))))))
                                     segs))]
    (iso-hull polys)))
