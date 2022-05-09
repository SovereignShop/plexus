(ns scad-paths.utils
  (:require
   [clojure.core.matrix :as mat]
   [scad-clj.model :as m]))

#_(mat/set-current-implementation :vectorz)

(def pi Math/PI)

(defn half [x] (/ x 2))

(defn rotation-vector
  "Rodrigues rotation formula"
  [v k a]
  (mat/add
   (mat/mmul (vec v)
             (Math/cos a))
   (mat/mmul (mat/cross k v)
             (Math/sin a))
   (mat/mmul (vec k)
             (mat/dot k v)
             (- 1 (Math/cos a)))))

(defn rotation-matrix [m]
  (mat/select m :butlast :butlast))

(defn translation-vector [m]
  (mat/select m :butlast 3))

(defn update-rotation
  ([m f]
   (as-> (f (rotation-matrix m)) m*
     (mat/join-along 1 m* (mat/column-matrix (translation-vector m)))
     (mat/join-along 0 m* [0 0 0 0])))
  ([m f a]
   (as-> (f (rotation-matrix m) a) m*
     (mat/join-along 1 m* (mat/column-matrix (translation-vector m)))
     (mat/join-along 0 m* [0 0 0 0])))
  ([m f a b]
   (as-> (f (rotation-matrix m) a b) m*
     (mat/join-along 1 m* (mat/column-matrix (translation-vector m)))
     (mat/join-along 0 m* [0 0 0 0]))))

(defn yaw*
  ([[vx vy vz] a]
   [(rotation-vector vx vy a)
    vy
    (rotation-vector vz vy a)]))

(defn yaw [m a]
  (update-rotation m yaw* a))

(defn pitch*
  ([[vx vy vz] a]
   [vx
    (rotation-vector vy vx a)
    (rotation-vector vz vx a)]))

(defn pitch [m a]
  (update-rotation m pitch* a))

(defn roll*
  ([[vx vy vz] a]
   [(rotation-vector vx vz a)
    (rotation-vector vy vz a)
    vz]))

(defn roll [m a]
  (update-rotation m roll* a))

(defn go-forward
  ([m x]
   (go-forward m x :z))
  ([m x axis]
   (let [v  (case axis
              :x (mat/select m 0 :butlast)
              :y (mat/select m 1 :butlast)
              :z (mat/select m 2 :butlast))
         t  (mat/select m :all 3)
         tr (mat/add t (mat/mmul (mat/conjoin-along 0 v 0) x))]
     (mat/join-along
      1
      (mat/select m :all :butlast)
      (mat/column-matrix tr)))))

(defn go-backward [m x]
  (let [v  (mat/select m 2 :butlast)
        t  (mat/select m :all 3)
        tr (mat/sub t (mat/mmul (mat/conjoin-along 0 v 0) x))]
    (mat/join-along
     1
     (mat/select m :all :butlast)
     (mat/column-matrix tr))))

(defn rotate33
  [[vx vy vz :as m] axis a]
  (if (vector? axis)
    [(rotation-vector vx axis a)
     (rotation-vector vy axis a)
     (rotation-vector vz axis a)]
    (case axis
      :x (pitch* m a)
      :y (yaw* m a)
      :z (roll* m a))))

(defn rotate [m axis a]
  (update-rotation m rotate33 axis a))

(defn translate [m p]
  (let [tr (mat/select m :all 3)]
    (mat/join-along
     1
     (mat/select m :all :butlast)
     (mat/column-matrix (mat/add tr (mat/conjoin-along 0 p 0))))))

(defn set-translation
  ([m v]
   (mat/set-selection m 3 :butlast v))
  ([m v axis]
   (case axis
     :x (mat/set-selection m 0 3 v)
     :y (mat/set-selection m 1 3 v)
     :z (mat/set-selection m 2 3 v))))

(defn invert [m]
  (mat/inverse (mat/matrix m)))

(defn bAc->a
  "side,angle,side -> side via. law of cosines."
  [b A c]
  (Math/sqrt (- (+ (Math/pow b 2)
                   (Math/pow c 2))
                (* 2 b c (Math/cos A)))))

(defn angle-between [a b]
  (Math/acos (/ (mat/dot a b) (* ( mat/magnitude a) (mat/magnitude b)))))

(defn about-equal? [v1 v2]
  (loop [[x & xs] v1
         [y & ys] v2]
    (cond
      (nil? x) true

      (< (Math/abs (- x y)) 0.00001)
      (recur xs ys)

      :else false)))

(defn opposing? [v1 v2]
  (about-equal? (mat/add v1 v2) [0 0 0]))

(defn rotation-axis-and-angle [v1 v2 opp]
  (let [cross (mat/cross v1 v2)]
    (if (about-equal? cross [0 0 0])
      (if (opposing? v1 v2)
        [pi opp]
        [0 opp])
      [(angle-between v1 v2) (mat/normalise cross)])))

(def identity-mat [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 0]])

(defn distance [a b]
  (Math/cbrt
   (apply + (mat/mul (mat/sub a b)
                     (mat/sub a b)))))

(defn ->inverse-scad-transform
  ([m] (->inverse-scad-transform identity-mat m))
  ([m1 m2]
   (let [translation (mat/sub (translation-vector m2) (translation-vector m1))
         a (rotation-matrix m1)
         b (rotation-matrix m2)
         [angle ortho] (rotation-axis-and-angle (nth a 0) (nth b 0) [0 0 1])
         c (rotate33 a ortho angle)
         [angle-2 ortho-2] (rotation-axis-and-angle (nth b 1) (nth c 1) (nth c 0))]
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
          shapes (for [idx (range 0 (inc steps))]
                   (if step-fn (step-fn idx model) block))]
      [(last shapes)
       (->> (for [[idx shape] (map list (range (inc steps)) shapes)]
              (->> (m/with-fn face-number shape)
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

(defn iso-hull [segs]
  (let [polys (map (fn [seg]
                     (let [m (meta seg)
                           tf (:start-transform m)
                           shape (:shape m)
                           pts (:points (second shape))]
                       (for [[x y] pts]
                         (-> tf
                             (go-forward x :x)
                             (go-forward y :y)
                             (translation-vector)))))
                   segs)

        first-poly (first polys)
        poly-res (count first-poly)
        n-indices (* (count polys) poly-res)
        indices (vec (range n-indices))
        bottom-face (take poly-res indices)
        top-face (subvec indices (- n-indices poly-res) n-indices)
        iter (apply mapv list (partition poly-res indices))
        side-faces (for [[l r] (partition 2 1 (conj iter (nth iter 0)))
                         [[v1 v2] [v3 v4]] (partition 2 1 (map list l r))
                         face [[v3 v4 v2] [v3 v2 v1]]]
                     face)]
    (m/polyhedron (sequence cat polys)
                  (list* bottom-face
                         (rseq top-face)
                         side-faces))))
