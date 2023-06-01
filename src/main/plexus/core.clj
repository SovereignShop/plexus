(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require
   [plexus.utils :as u]
   [plexus.schema :as schema :refer [validate-form]]
   [clj-manifold3d.core :as m]
   [malli.core :as ma]
   [plexus.impl :as impl]))

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
        extrude* (map (fn [form]
                        (assoc form :to (:models opts)))
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

(defn lookup-transform [])

(defn lookup-property [])

(defn extrude [& forms]
  (impl/extrude forms))

(defn subtract [a & args]
  #_(let [all-modules (reduce (fn [ret arg]
                              (if (impl/path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (impl/get-models (cons a args))
        result (apply m/difference all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defn intersect [a & args]
  #_(let [all-modules (reduce (fn [ret arg]
                              (if (impl/path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (impl/get-models (cons a args))
        result (apply m/intersection all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defn join [a & args]
  #_(let [all-modules (reduce (fn [ret arg]
                              (if (impl/path? arg)
                                (merge ret (-> arg meta :modules))
                                (merge ret (-> arg second meta :modules))))
                            {}
                            (cons a args))
        all-results (impl/get-models (cons a args))
        result (apply m/union all-results)]
    (with-meta
      (conj (vec (vals all-modules)) result)
      (assoc (meta a) :modules all-modules :result result))))

(defmacro points
  [& forms]
  (let [[opts forms*] (impl/parse-path forms)
        axes (or (:axes opts) [:x :y])
        meta-props (:meta-props opts)
        sym (gensym "tv-")
        clauses (for [axis axes]
                  (case axis
                    :x `(nth ~sym 0)
                    :y `(nth ~sym 1)
                    :z `(nth ~sym 2)))]
    `(let [p# (impl/extrude ~forms*)]
       (impl/path-points p# (fn [~sym]
                              (vector ~@clauses)) ~meta-props))))

(defmacro defmodel [name & path]
  `(def ~@name (impl/extrude path)))

(comment

  (-> (m/cross-section (points
                        :axes [:x :y]
                        (frame :name :origin :fn 10)
                        (translate :x 50)
                        (left :angle (* 2 Math/PI) :curve-radius 50)))
      (m/extrude 100)
      (m/get-mesh)
      (m/export-mesh "test.glb"))


  (-> (time (-> (:c (:frames (extrude
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
                              (for [_ (range 12)]
                                [(forward :length 20)
                                 (down :angle (/ Math/PI 1) :curve-radius 20)
                                 (forward :length 20)
                                 (up :angle (/ Math/PI 1) :curve-radius 20)]))))
                (m/get-mesh)))
      (m/export-mesh "test.glb"))

  )
