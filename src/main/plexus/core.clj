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
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/left ~@opts) schema/curve-schema))

(defn right
    "Extrude an ego-centric right curve. Rotate-extrudes about a y-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/right ~@opts) schema/curve-schema))

(defn up
    "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric y axis by negative `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/up ~@opts) schema/curve-schema))

(defn down
  "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/down ~@opts) schema/curve-schema))

(defn arc [& opts]
  (validate-form  `(:plexus.impl/arc ~@opts) schema/curve-schema))

(defn roll
  "Depricated. Use `(rotate :z rad)` instead.

  Rotates the current transform set by `:angle` radians about the z axis.

  opts

  `:angle` - Radians of roll"
  [& opts]
  (validate-form `(:plexus.impl/roll ~@opts) schema/curve-schema))

(defn forward
  "Extrude the model set forward.

  opts:

  `:forward` - Extrude in cannonical direction (along z axis)
  `:x` - extrude along x axis
  `:y` - extrude along y axis
  `:z` - extrude along z axis
  `:twist` - Twist radians while extruding. End transform matches the degree of twist.
  `:center` - (default: false) whether extrusion should be centered at starting transform frame.
  `:branch?` - (default: false) whether extrusion should be applied in a branch without updating
               the transform.
  `:n-steps` - Number of steps in the extrusion.
  `:transform-step-fn` - A function (fn [tf i] tf) that is applied to each step. Note, this does not
                         currently effect extrusion but is usefull for generating vertices when using
                        `points`. "
  [& opts]
  (validate-form `(:plexus.impl/forward ~@opts) schema/forward-schema))

(defn backward
  "Depreicated. Use `forward` with negative values instead."
  [& opts]
  (validate-form `(:plexus.impl/backward ~@opts) schema/linear-extrude-schema))

(defn loft
  "Loft between segments. All lofted cross-sections must have equal number of points. Optional
  `:to` parameter specifies which frames to loft."
  [& opts]
  (validate-form `(:plexus.impl/loft ~@opts) schema/any-map-schema))

(defn hull
  "Makes a convex hull out of wrapped segments. Optional `:to` parameter specifies which
  frames to loft."
  [& opts]
  (validate-form `(:plexus.impl/hull ~@opts) schema/any-map-schema))

(defn model [& args]
  (validate-form `(:plexus.impl/model ~@args) schema/model-schema))

(defn translate
  "Translate frame. Takes `:x`, `:y`, and/or `:z` parameters and applies the translation
  relative to the frame transform. If :global is set, axis values set relative to global origin."
  [& args]
  (validate-form `(:plexus.impl/translate ~@args) schema/translate-schema))

(defn mirror
  "Only applies to result expressions. Required :normal parameter specifies
  the mirror plane."
  [& args]
  (validate-form `(:plexus.impl/mirror ~@args) schema/mirror-schema))

(defn rotate
  "Rotates around `:x`, `:y`, and/or `:z` axes relative to the current frame(s) transformation."
  [& args]
  (validate-form `(:plexus.impl/rotate ~@args) schema/rotate-schema))

(defn transform
  "Currently only supports `:replace` parameter, which replaces the current frames transformation."
  [& args]
  (validate-form `(:plexus.impl/transform ~@args) schema/transform-schema))

(defn spin
  "extrude by spinning in place."
  [& args]
  (validate-form `(:plexus.impl/spin ~@args) schema/any-map-schema))

(defn set
  "Set default args for active frames, or explicity with the `:to [:frame1 :frame2 ...]` parameter."
  [& args]
  (validate-form `(:plexus.impl/set ~@args) schema/any-map-schema))

(defn set-meta
  "Set meta-data on the current frame. Segments inherent metadata from the frame they are associated with."
  [& args]
  (validate-form `(:plexus.impl/set-meta ~@args) schema/any-map-schema))

(defn branch
  "Branch off from a frames current transform. Required :from parameter specifies which frame to branch off from.
  The body of the branch is the same as (extrude ...)."
  [& args]
  (validate-form `(:plexus.impl/branch ~@args) schema/branch-schema))

(defn segment
  [& args]
  (validate-form `(:plexus.impl/segment ~@args) schema/any-map-schema))

(defn to
  "Body is applied only to frames specified in the required `:models [:frame ...]` paramter"
  [& p]
  (let [[opts parsed-path] (impl/parse-path p)
        extrude* (map (fn [form]
                        (assoc form :to (:models opts)))
                      parsed-path)]
    (segment extrude*)))

(defn mask [& args]
  (validate-form `(:plexus.impl/model ~@(conj (vec args) :mask? true)) schema/model-schema))

(defn frame [& args]
  (validate-form `(:plexus.impl/frame ~@args) schema/frame-schema))

(defn save-transform
  "Save transform by name.

  opts

  `:frame` - The frame to save the current transform of.
  `:name` - Name of the saved transform."
  [& args]
  (validate-form `(:plexus.impl/save-transform ~@args) schema/save-transform-schema))

(defn offset
  "offset cross-sections.

  opts

  `:deta` - number of mm to offset, positive or negative.
  `:join-type` - Type of corners to create when offsetting. Options are :square, :round, or :miter.
  `:simplify` - Remove vertices from resulting contours in cross-section that are less than the
                specified distance epsilon from an imaginary line that passes through its two adjacent vertices."
  [& args]
  (validate-form `(:plexus.impl/offset ~@args) schema/offset-schema))

(defn add-ns
  "Namespace frames. If a namespace already exists, it will be be extended (period separated).

  opts

  `:namespace` - string or keyword."
  [& args]
  (validate-form `(:plexus.impl/add-ns ~@args) schema/add-ns-schema))

(defn result
  "Combines frames together with CSG and other 3D geometric operations. Supports operations
  `union`, `difference`, `intersection`, `translate`, `rotate`, `mirror`, `loft`, `hull`,
   and `trim-by-plane`. Frames in result trees are specified by name. Results can also include
   other results (also by name).

  opts

  :name - name of the result.
  :expr - Tree of SCG and other 3D geometric operations."
  [& args]
  (validate-form `(:plexus.impl/result ~@args) schema/result-schema))

(defn union
  "For use in result expressions. Unions frames."
  [& args]
  (validate-form `(:plexus.impl/union ~@args) schema/result-op-schema))

(defn intersection
  "For use in result expressions. Intersects frames."
  [& args]
  (validate-form `(:plexus.impl/intersection ~@args) schema/result-op-schema))

(defn difference
  "For use in result expressions. Subtracts frames."
  [& args]
  (validate-form `(:plexus.impl/difference ~@args) schema/result-op-schema))

#_(defn slice
  "extrudes the end slice of the frames according to the normal defined by the z-axis. Useful
  for hulls and lofts."
  [& args]
  (validate-form `(:plexus.impl/slice ~@args) [:map [:length number?]]))

#_(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args) schema/import-schema))

(defn insert
  "insert an extrusion at the current position and orientation.

  opts

  `:extrusion` - extrusion to insert.
  `:models` (optional) - models to inserts. Can include results and/or frames. Defaults to singleton vector of last result model.
  `:ns` (optional) - an optional namespace ot associate with inserted models.
  `:end-frame` (optional) - name of a inserted the next segment is relative to. The frame must be defined in the provided extrusion."
  [& args]
  (validate-form `(:plexus.impl/insert ~@args) schema/any-map-schema))

#_(defn show-coordinate-frame [& args]
  (validate-form `(:plexus.impl/show-coordinate-frame ~@args) schema/show-coordinate-frames-schema))

(defn trim-by-plane
  "For use in result expressions. Trims a model by subracting the halfspace defined by the `:normal`.

  :opts

  `:normal` - [x y z] defines the normal to the plane, points in the direction of the subtracted halfspace.
  `:origin-offset` - Offset of the normal from [0 0 0] in the direction of the normal."
  [& args]
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

(defn lookup-transform
  "lookup saved transform by name. See `save-transform`."
  [extrusion key]
  (-> extrusion :transforms key))

(defn get-frame
  "Get frame from `extrusion` by `name`"
  [extrusion name]
  (-> extrusion :frames name))

(defn get-model
  "Get model from `extrusion` by `name`. Models are composited frames. For every frame, a model
  is created by unioning the segments. Also, every result expression yields a model."
  [extrusion name]
  (-> extrusion :models name))

(defn extrude
  "the main entry point. Takes a sequence of extrusion forms. Forms are automatically flattened."
  [& forms]
  (impl/extrude forms))

(defmacro points
  "Similar to extrude "
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
           (let [manifold (cond-> (get (:models m) (:main-model m))
                            (#{"3ds"} file-ext) (m/rotate [-90 0 0]))]
             (m/export-mesh (m/get-mesh manifold) (format "out/%s/%s" file-ext filename)))))))))

(defn export
  ([model filename]
   (export model filename (m/material)))
  ([model filename material]
   (m/export-mesh (m/get-mesh (impl/to-manifold model)) filename :material material)))
