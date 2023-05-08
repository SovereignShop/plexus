(ns plexus.transforms
  (:require
   [clojure.core.matrix :as mat]))

(defrecord Transform [dir pos])

(defn rodrigues-rotation
  [v k a]
  (mat/add
   (mat/mmul (vec v)
             (Math/cos a))
   (mat/mmul (mat/cross k v)
             (Math/sin a))
   (mat/mmul (vec k)
             (mat/dot k v)
             (- 1 (Math/cos a)))))

(defn yaw
  ([{[vx vy vz] :dir :as m} a]
   (let [d [(rodrigues-rotation vx vy a)
            vy
            (rodrigues-rotation vz vy a)]]
     (assoc m :dir d))))

(defn pitch
  ([{[vx vy vz] :dir :as m} a]
   (let [d [vx
            (rodrigues-rotation vy vx a)
            (rodrigues-rotation vz vx a)]]
     (assoc m :dir d))))

(defn roll
  ([{[vx vy vz] :dir :as m} a]
   (let [d [(rodrigues-rotation vx vz a)
            (rodrigues-rotation vy vz a)
            vz]]
     (assoc m :dir d))))

(defn transform? [x]
  (instance? Transform x))

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
        [angle _] (rotation-axis-and-angle [1 0 0] (nth rm 0) [0 0 1])]
    angle))

(defn rotate [{[vx vy vz] :dir :as transform} axis a]
  (if (sequential? axis)
    (let [d [(rodrigues-rotation vx axis a)
             (rodrigues-rotation vy axis a)
             (rodrigues-rotation vz axis a)]]
      (assoc transform :dir d))
    (case axis
      :x (pitch transform a)
      :y (yaw transform a)
      :z (roll transform a))))

(defn go-forward
  ([tf x]
   (go-forward tf x :z))
  ([{:keys [dir pos] :as transform} x axis]
   (let [v  (mat/select dir (case axis :x 0 :y 1 :z 2) :all)
         tr (mat/add pos (mat/mmul v x))]
     (assoc transform :pos tr))))

(defn go-backward
  ([tf x]
   (go-forward tf x :z))
  ([{:keys [dir pos] :as transform} x axis]
   (let [v  (mat/select dir (case axis :x 0 :y 1 :z 2) :all)
         tr (mat/sub pos (mat/mmul v x))]
     (assoc transform :pos tr))))

(defn set-translation
  ([transform v]
   (assoc transform :pos v))
  ([transform v axis]
   (update transform :pos mat/set-selection (case axis :x 0 :y 1 :z 2) v)))

(defn translation-vector [tf]
  (:pos tf))

(defn rotation-matrix [tf]
  (:dir tf))

(defn pos [t]
  (:pos t))

(defn dir [t]
  (:dir t))

(defn x [tf]
  (-> tf :pos (nth 0)))

(defn y [tf]
  (-> tf :pos (nth 1)))

(defn z [tf]
  (-> tf :pos (nth 2)))

(defn rx [tf]
  (-> tf :dir (nth 0)))

(defn ry [tf]
  (-> tf :dir (nth 1)))

(defn rz [tf]
  (-> tf :dir (nth 2)))

(def identity-rot (mat/matrix [[1 0 0]
                               [0 1 0]
                               [0 0 1]]))

(def identity-pos (mat/matrix [0 0 0]))

(mat/select identity-rot 0 :all)

(def identity-tf
  (->Transform identity-rot identity-pos))

(defn transform
  ([]
   (transform identity-rot identity-pos))
  ([dir pos]
   (->Transform dir pos)))

