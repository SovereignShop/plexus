(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require
   [plexus.utils :as u]
   [plexus.schema :as schema :refer [validate-form]]
   [clj-manifold3d.core :as m]
   [malli.core :as ma]
   [plexus.impl :as impl]))

(defn extrude-to
  [& opts]
  `(:plexus.impl/extrude-to ~@opts))

(defn left
  "Extrude an ego-centric left curve. Rotate-extrudes about a y-axis offset
  along the ego-centric x axis by negative `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/left ~@opts) schema/curve-schema))

(defn right
    "Extrude an ego-centric right curve. Rotate-extrudes about a y-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/right ~@opts) schema/curve-schema))

(defn up
    "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric y axis by negative `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/up ~@opts) schema/curve-schema))

(defn down
  "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/down ~@opts) schema/curve-schema))

(defn arc [& opts]
  (validate-form  `(:plexus.impl/arc ~@opts) schema/curve-schema))

(defn roll
  "Depricated. Use `(rotate :z rad)` instead.

  Rotates the current transform set by `:angle` radians about the z axis.

  opts
  :angle - Radians of roll"
  [& opts]
  (validate-form `(:plexus.impl/roll ~@opts) schema/curve-schema))

(defn forward
  "Extrude the model set forward.

  opts:

  :forward - Extrude in cannonical direction (along z axis)
  :x - extrude along x axis
  :y - extrude along y axis
  :z - extrude along z axis
  :twist - Twist radians while extruding. End transform matches the degree of twist.
  :center - (default: false) whether extrusion should be centered at starting transform frame.
  :branch? - (default: false) whether extrusion should be applied in a branch without updating
             the transform.
  :n-steps - Number of steps in the extrusion.
  :transform-step-fn - A function (fn [tf i] tf) that is applied to each step. Note, this does not
                       currently effect extrusion but is usefull for generating vertices when using
                       `points`. "
  [& opts]
  (validate-form `(:plexus.impl/forward ~@opts) schema/forward-schema))

(defn backward
  "Depreicated. Use `forward` with negative values instead."
  [& opts]
  (validate-form `(:plexus.impl/backward ~@opts) schema/linear-extrude-schema))

(defn hull
  [& opts]
  (validate-form `(:plexus.impl/hull ~@opts) schema/any-map-schema))

(defn model [& args]
  (validate-form `(:plexus.impl/model ~@args) schema/model-schema))

(defn translate [& args]
  (validate-form `(:plexus.impl/translate ~@args) schema/translate-schema))

(defn rotate [& args]
  (validate-form `(:plexus.impl/rotate ~@args) schema/rotate-schema))

(defn transform [& args]
  (validate-form `(:plexus.impl/transform ~@args) schema/transform-schema))

(defn spin [& args]
  (validate-form `(:plexus.impl/spin ~@args)
                 schema/any-map-schema))

(defn set [& args]
  (validate-form `(:plexus.impl/set ~@args)
                 schema/any-map-schema))

(defn branch [& args]
  (validate-form `(:plexus.impl/branch ~@args) schema/branch-schema))

(defn segment [& args]
  (validate-form `(:plexus.impl/segment ~@args) schema/any-map-schema))

(defn to [& p]
  (let [[opts parsed-path] (impl/parse-path p)
        extrude* (map (fn [[x & xs]]
                     (list* x :to (:models opts) xs))
                   parsed-path)]
    (segment extrude*)))

(defn mask [& args]
  (validate-form `(:plexus.impl/model ~@(conj (vec args) :mask? true)) schema/model-schema))

(defn frame [& args]
  (validate-form `(:plexus.impl/frame ~@args) schema/frame-schema))

(defn save-transform [& args]
  (validate-form `(:plexus.impl/save-transform ~@args) schema/save-transform-schema))

(defn offset [& args]
  (validate-form `(:plexus.impl/offset ~@args) schema/offset-schema))

(defn minkowski [& args]
  (validate-form `(:plexus.impl/minkowski ~@args) schema/any-map-schema))

(defn add-ns [& args]
  (validate-form `(:plexus.impl/add-ns ~@args) schema/add-ns-schema))

#_(defn forward-until [& args]
  (validate-form `(:plexus.impl/forward-until ~@args) schema/any-map-schema))

(defn ignore [& args]
  (validate-form `(:plexus.impl/ignore ~@args) schema/any-map-schema))

(defn result [& args]
  (validate-form `(:plexus.impl/result ~@args) schema/result-schema))

(defn union [& args]
  (ma/coerce schema/result-op-schema `(:plexus.impl/union ~@args) nil {:registry u/registry}))

(defn intersection [& args]
  (ma/coerce schema/result-op-schema `(:plexus.impl/intersection ~@args) nil {:registry u/registry}))

(defn difference [& args]
  (ma/coerce schema/result-op-schema `(:plexus.impl/difference ~@args) nil {:registry u/registry}))

(defn slice [& args]
  (validate-form `(:plexus.impl/slice ~@args) [:map [:length number?]]))

(defn iso-hull [& args]
  (validate-form `(:plexus.impl/iso-hull ~@args) schema/hull-schema))

(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args) schema/import-schema))

(defn show-coordinate-frame [& args]
  (validate-form `(:plexus.impl/show-coordinate-frame ~@args) schema/show-coordinate-frames-schema))

(defn pattern [& args]
  (let [{:keys [from axis distances angles namespaces end-at :plexus.impl/list]} (impl/parse-args (list* :na args))]
    (assert (or angles distances))
    (apply segment
           (sort-by #(= (first %) :plexus.impl/segment)
                    (for [[angle distance namespace idx]
                          (map vector
                               (or angles (repeat 0))
                               (or distances (repeat 0))
                               (or namespaces (repeat nil))
                               (range))]
                      (if (and end-at (= end-at idx))
                        (segment
                         (rotate axis angle)
                         (translate axis distance)
                         (add-ns :namespace namespace)
                         (segment list))
                        (branch
                         :from from
                         (rotate axis angle)
                         (translate axis distance)
                         (add-ns :namespace namespace)
                         (segment list))))))))

(comment


  ;; (1) accumulate defs.
  (defpath
    (def bolt-mask (frame :cross-section (m/circle 10)))
    (def b (frame :cross-section (m/circle 9)))

    (forward :length 20)

    (branch
     :from a
     (rotate :y (/ Math/PI 2))
     (forward :z 20))

    (branch
     :from a
     :with []
     (def c (difference a b))
     (rotate :y (- (/ Math/PI 2)))
     (forward :z 30))

    (def d (union c (difference a b))))


  (union a b c)

  ;; Accumulate the defs, turn them into (frame ...) forms, then just put the defs at the end that grab from results.
  (in-ns `plexus.impl)

  (ns plexus.core)

  (require '[plexus.wtf :as wtf])

  wtf/a

  plexus.wtf/a
  (def a 65)

  (plexus.impl/a 65)

  (defpath
    (ns )
    (def body (circle 20))
    (def mask (circle 18))

    (forward :length 20)
    (branch
     :from body
     (left :angle (/ Math/PI 2) :curve-radius 15)
     (forward :length 20))
    (branch
     :from body
     (right :angle (- (/ Math/PI 2)) :curve-radius 15)
     (forward :length 20)
     (def other-body (square 10 10 true))
     (forward :z 50 :to [other-body])))

  (def result (difference body mask))


  (def  50)

  (def result (difference a b c))


  (defmacro tmp
    [def-form]
    (let [sym (second def-form)]
      `(let [~sym 20]

         (def ~sym ~sym))))

  (subvec [1 2 3] 2)

  (tmp (def a 30))

  (time (-> (:c (impl/extrude
                 (result :name :c
                         :expr (->> (difference (union :a :d) :b)
                                    (translate :x 30 :y 70)))

                 (frame :name :a :cross-section (m/square 10 15 true))
                 (frame :name :b :cross-section (m/square 8 13 true))

                 (forward :length 20)
                 (left :angle Math/PI :curve-radius 20)
                 (forward :length 20)
                 (right :angle Math/PI :curve-radius 20)
                 (frame :name :d :cross-section (m/circle 25))
                 (for [_ (range 4)]
                   [(forward :length 20)
                    (down :angle (/ Math/PI 1) :curve-radius 20)
                    (forward :length 20)
                    (up :angle (/ Math/PI 1) :curve-radius 20)])))
            (m/get-mesh)
            (m/export-mesh "test.glb")))

  (-> (:c (impl/extrude
           (def a (extrusion :cross-section (m/circle 3)))
           (def b (extrusion :cross-section (m/circle 2)))
           (forward :length 10)
           (left :angle pi|2 :curve-radius 4)
           (forward :length 30)
           (left :angle pi|2 :curve-radius 4)
           (branch
            :from a
            :with []
            (def a (frame :cross-section (m/circle 10)))
            (def c (frame :cross-section (m/square 20 20)))

            (rotate :y (- (/ Math/PI 2)))
            (forward :length 20)
            (translate :z 20 :to [a b c])
            (forward :length 5))
           (translate :z 10 :y 30 :x 15)
           (rotate :x (/ Math/PI 6))
           (translate :z -0.1 :to [:b])
           (forward :length 20)
           (forward :length 0.2 :to [:b])

           (def d (difference a b c))
           (def e ()))
          (def result))
      (m/get-mesh)
      (m/export-mesh "test.stl"))

  (-> (u/iso-hull [[[-10 0 0] [10 0 0] [0 10 0]]
                   [[-10 0 20] [50 0 20] [0 20 20]]])
      (m/get-mesh)
      (m/export-mesh "test.stl"))

  (forward :length 20 :to [:a :b :c])
  (forward :length 200 :to [:x :y :z])

  (forward :z 400 :to [:body :mask])

  (forward :z 500 :to [:body :mask])
  (forward :z 650 :to [:body :mask])
  (branch
   :from :body
   :with []
   (forward :z 20))

  )
