(ns plexus.transforms
  (:import
   [manifold3d.glm MatrixTransforms DoubleMat4x3 DoubleVec3])
  (:require
   [clojure.core.matrix :as mat]))

(defn yaw
  ([m a]
   (MatrixTransforms/Yaw m a)))

(defn pitch
  ([m a]
   (MatrixTransforms/Pitch m a)))

(defn roll
  ([m a]
   (MatrixTransforms/Roll m a)))

(defn transform? [x]
  (instance? DoubleMat4x3 x))

(defn- about-equal? [v1 v2]
  (loop [[x & xs] v1
         [y & ys] v2]
    (cond
      (nil? x) true

      (< (abs (- x y)) 0.00001)
      (recur xs ys)

      :else false)))

(defn- opposing? [v1 v2]
  (about-equal? (mat/add v1 v2) [0 0 0]))

(defn angle-between [a b]
  (Math/acos (/ (mat/dot a b) (* ( mat/magnitude a) (mat/magnitude b)))))

(defn rotation-axis-and-angle [v1 v2 opp]
  (let [cross (mat/cross v1 v2)]
    (if (about-equal? cross [0 0 0])
      (if (opposing? v1 v2)
        [Math/PI opp]
        [0 opp])
      [(angle-between v1 v2) (mat/normalise cross)])))

(defn get-roll
  [tf]
  (let [rm (:dir tf)
        [angle _] (rotation-axis-and-angle [1 0 0] (nth rm 0) [0 0 1](assoc :manifold (.transform manifold (MatrixTransforms/InvertTransform m))))]
    angle))

(defn rotate [m axis a]
  (MatrixTransforms/Rotate m axis a))

(defn go-forward
  ([m x]
   (MatrixTransforms/Translate m (DoubleVec3. 0 0 x)))
  ([m x axis]
   (MatrixTransforms/Translate m (case axis
                                   :x (DoubleVec3. x 0 0)
                                   :y (DoubleVec3. 0 x 0)
                                   :z (DoubleVec3. 0 0 x)))))

(defn go-backward
  ([m x]
   (go-forward m (- x)))
  ([m x axis]
   (go-backward m (- x) axis)))

(defn set-translation
  ([m [x y z]]
   (DoubleMat4x3. (.getColumn ^DoubleVec3 m 0)
                  (.getColumn ^DoubleVec3 m 1)
                  (.getColumn ^DoubleVec3 m 2)
                  (DoubleVec3. x y z)))
  ([m v axis]
   (let [^DoubleVec3 tr (.getColumn m 3)]
     (DoubleMat4x3. (.getColumn ^DoubleVec3 m 0)
                    (.getColumn ^DoubleVec3 m 1)
                    (.getColumn ^DoubleVec3 m 2)
                    (case axis
                      :x (DoubleVec3. v (.y tr) (.z tr))
                      :y (DoubleVec3. (.x tr) v (.z tr))
                      :z (DoubleVec3. (.x tr) (.y tr) v))))))

(defn translation-vector [m]
  (let [tr (.getColumn m 3)]
    [(.x tr) (.y tr) (.z tr)]))

(def identity-tf
  (DoubleMat4x3. 1))

(defn transform
  ([]
   (DoubleMat4x3. 1)))
