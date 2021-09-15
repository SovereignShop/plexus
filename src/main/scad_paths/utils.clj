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

(defn go-forward [m x]
  (let [v  (mat/select m 2 :butlast)
        t  (mat/select m :all 3)
        tr (mat/add t (mat/mmul (mat/conjoin-along 0 v 0) x))]
    (mat/join-along
     1
     (mat/select m :all :butlast)
     (mat/column-matrix tr))))

(defn rotate
  [[vx vy vz] axis a]
  [(rotation-vector vx axis a)
   (rotation-vector vy axis a)
   (rotation-vector vz axis a)])

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

(defn- rotation-axis-and-angle [v1 v2 opp]
  (let [cross (mat/cross v1 v2)]
    (if (about-equal? cross [0 0 0])
      (if (opposing? v1 v2)
        [pi opp]
        [0 opp])
      [(angle-between v1 v2) (mat/normalise cross)])))

(def identity-mat [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 0]])

(defn ->scad-transform
  "Make an OpenSCAD transformation function between two coordinate frames."
  ([m] (->scad-transform identity-mat m))
  ([m1 m2]
   (let [translation (mat/sub (translation-vector m2) (translation-vector m1))
         a (rotation-matrix m1)
         b (rotation-matrix m2)
         [angle ortho] (rotation-axis-and-angle (nth a 0) (nth b 0) [0 0 1])
         c (rotate a ortho angle)
         [angle-2 ortho-2] (rotation-axis-and-angle (nth b 1) (nth c 1) (nth c 0))]
     (comp
      (partial m/translate translation)
      (partial m/rotatev (- angle-2) ortho-2)
      (partial m/rotatev angle ortho)))))

(defn ->inverse-scad-transform
  ([m] (->scad-transform identity-mat m))
  ([m1 m2]
   (let [translation (mat/sub (translation-vector m2) (translation-vector m1))
         a (rotation-matrix m1)
         b (rotation-matrix m2)
         [angle ortho] (rotation-axis-and-angle (nth a 0) (nth b 0) [0 0 1])
         c (rotate a ortho angle)
         [angle-2 ortho-2] (rotation-axis-and-angle (nth b 1) (nth c 1) (nth c 0))]
     (comp
      (partial m/rotatev (- angle-2) ortho-2)
      (partial m/rotatev angle ortho)
      (partial m/translate translation)))))
