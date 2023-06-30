(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require
   [clojure.java.io :as io]
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

(defn loft
  [& opts]
  (validate-form `(:plexus.impl/loft ~@opts) schema/any-map-schema))

(defn hull
  [& opts]
  (validate-form `(:plexus.impl/hull ~@opts) schema/any-map-schema))

(defn model [& args]
  (validate-form `(:plexus.impl/model ~@args) schema/model-schema))

(defn translate [& args]
  (validate-form `(:plexus.impl/translate ~@args) schema/translate-schema))

(defn mirror [& args]
  (validate-form `(:plexus.impl/mirror ~@args) schema/mirror-schema))

(defn rotate [& args]
  (validate-form `(:plexus.impl/rotate ~@args) schema/rotate-schema))

(defn transform [& args]
  (validate-form `(:plexus.impl/transform ~@args) schema/transform-schema))

(defn spin [& args]
  (validate-form `(:plexus.impl/spin ~@args) schema/any-map-schema))

(defn set [& args]
  (validate-form `(:plexus.impl/set ~@args) schema/any-map-schema))

(defn set-meta [& args]
  (validate-form `(:plexus.impl/set-meta ~@args) schema/any-map-schema))

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
  (validate-form `(:plexus.impl/union ~@args) schema/result-op-schema))

(defn intersection [& args]
  (validate-form `(:plexus.impl/intersection ~@args) schema/result-op-schema))

(defn difference [& args]
  (validate-form `(:plexus.impl/difference ~@args) schema/result-op-schema))

(defn slice [& args]
  (validate-form `(:plexus.impl/slice ~@args) [:map [:length number?]]))

(defn iso-hull [& args]
  (validate-form `(:plexus.impl/iso-hull ~@args) schema/hull-schema))

(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args) schema/import-schema))

(defn insert [& args]
  (validate-form `(:plexus.impl/insert ~@args) schema/any-map-schema))

(defn show-coordinate-frame [& args]
  (validate-form `(:plexus.impl/show-coordinate-frame ~@args) schema/show-coordinate-frames-schema))

(defn trim-by-plane [& args]
  (validate-form `(:plexus.impl/trim-by-plane ~@args) schema/any-map-schema))

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

(defn lookup-transform [extrusion key]
  (-> extrusion :transforms key))

(defn get-frame [extrusion name]
  (-> extrusion :frames name))

(defn get-model [extrusion name]
  (-> extrusion :models name))

(defn lookup-property [])

(defn extrude [& forms]
  (impl/extrude forms))

(defn subtract [a & args])

(defn intersect [a & args])

(defn join [a & args])

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
                              (vector ~@clauses))
                         ~meta-props))))

(defmacro defmodel [name & path]
  `(def ~@name (impl/extrude path)))

(defn export-models
  "Export Manifold and Extrusion vars in `namespace` using `file-ext` format."
  [namespace file-ext]
  (-> (io/file (format "out/%s" file-ext)) (.mkdirs))
  (doall
   (for [[_ x] (ns-map namespace)
         :let [var-meta (meta x)]
         :when (:export-model var-meta)]
     (future
       (let [filename (-> var-meta :name name (str (format ".%s" file-ext)))
             m @x]
         (if (m/manifold? m)
           (m/export-mesh (m/get-mesh (cond-> m (= file-ext "3ds") (m/rotate [-90 0 0]))) (format "out/%s/%s" file-ext filename))
           (let [manifold (cond-> (get (:frames m) (:main-frame m))
                            (#{"3ds"} file-ext) (m/rotate [-90 0 0]))]
             (m/export-mesh (m/get-mesh manifold) (format "out/%s/%s" file-ext filename)))))))))

(defn export [model filename]
  (m/export-mesh (m/get-mesh (impl/to-manifold model)) filename))
