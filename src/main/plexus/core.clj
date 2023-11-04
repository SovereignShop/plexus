(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require
   [clojure.java.io :as io]
   [plexus.utils :as u]
   [plexus.schema :as schema :refer [validate-form]]
   [clj-manifold3d.core :as m]
   [malli.core :as ma]
   [plexus.impl :as impl]))

(defn- pretty-demunge
  [fn-object]
  (let [dem-fn (clojure.repl/demunge (str fn-object))
        pretty (second (re-find #"(.*?\/.*?)[\-\-|@].*" dem-fn))]
    (if pretty (peek (clojure.string/split pretty #"/")) dem-fn)))

(defn- get-map-key-schemas [schema]
  (if (vector? schema)
    (if (= (first schema) :map)
      (next schema)
      (into [] cat (map get-map-key-schemas (next schema))))
    []))

(defn- render-arglist [schema]
  (let [map-key-schemas (get-map-key-schemas
                         (if (ma/schema? schema)
                           (ma/form schema)
                           schema))]
    (if (seq map-key-schemas)
      (mapcat (fn [s]
                (let [[k t] (if (= (count s) 2)
                              s
                              [(nth s 0) (nth s 2)])]
                  [k (symbol (pretty-demunge t))]))
              map-key-schemas)
      ['...])))

(defmacro defop
  {:arglists '([op-name doc schema])
   :clj-kondo/lint-as 'clojure.core/def}
  [op-name doc schema]
  (let [op-key (keyword "plexus.impl" (str op-name))
        arglists (list (vec (render-arglist (eval schema))))]
    `(defmacro ~op-name
       {:arglists '~arglists
        :doc ~doc}
       [& opts#]
       (let [form# ~'&form
             m# (assoc (meta form#) :file *file*)
             s# (var ~schema)]
         `(with-meta
            (plexus.schema/validate-form (list ~~op-key ~@opts#) (deref ~s#))
            ~m#)))))

(defop left
  "Extrude a left curve. Rotate-extrudes about a y-axis offset
  along the x axis by negative `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  schema/curve-schema)

(defop right
  "Extrude an right curve. Rotate-extrudes about a y-axis offset
  along the x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  schema/curve-schema)

(defop up
  "Extrude a up curve. Rotate-extrudes about a x-axis offset
  along the y axis by negative `:curve-radians`.

  opts
  `:angle` - number of radians to cufve.
  `:curve-radius` - radius of the curve."
  schema/curve-schema)

(defop down
  "Extrude a right curve. Rotate-extrudes about a x-axis offset
  along the x axis by `:curve-radians`.

  opts
  `:angle` - number of radians to curve.
  `:curve-radius` - radius of the curve."
  schema/curve-schema)

(defop curve
  "Extrude a curve segment.

  opts
  `:angle` number of radians to curve.
  `:curve-radius` radius of the curve.
  `:direction` :left, :right, :up, or :down direction. Default is :left.
  `:roll` number of radians to roll aboute Z before extruding."
  schema/curve-generic-schema)

(defop forward
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
  schema/forward-schema)

(defop loft
  "Loft between segments. All lofted cross-sections must have equal number of points. Optional
  `:to` parameter specifies which frames to loft."
  schema/any-map-schema)

(defop hull
  "Makes a convex hull out of wrapped segments. Optional `:to` parameter specifies which
  frames to loft."
  schema/any-map-schema)

(defop translate
  "Translate frame. Takes `:x`, `:y`, and/or `:z` parameters and applies the translation
  relative to the frame transform. If :global is set, axis values set relative to global origin."
  schema/translate-schema)

(defop mirror
  "Only applies to result expressions.

  opts:

  `:normal` [x y z] normal to the mirror plane."
  schema/mirror-schema)

(defop rotate
  "Rotates around `:x`, `:y`, and/or `:z` axes relative to the current frame(s) transformation."
  schema/rotate-schema)

(defop transform
  "Currently only supports `:replace` parameter, which replaces the current frames transformation."
  schema/transform-schema)

(defop set
  "Set default args for active frames, or explicity with the `:to [:frame1 :frame2 ...]` parameter."
  schema/any-map-schema)

(defop set-meta
  "Set meta-data on the current frame. Segments inherent metadata from the frame they are associated with."
  schema/any-map-schema)

(defop branch
  "Branch off from a frames current transform.

  opts:
 `:from` (required) parameter specifies which frame to branch off from.
         The body of the branch is the same as (extrude ...).
  `:with` (optional) Specifies what subset of current frames to include in the
          branch context."
  schema/branch-schema)

(defn to
  "Body is applied only to frames specified in the required `:models [:frame ...]` paramter"
  [& p]
  (let [[opts parsed-path] (impl/parse-path p)
        extrude* (map (fn [form]
                        (assoc form :to (:models opts)))
                      parsed-path)]
    [extrude*]))

(defop frame
  "Add a new frame to the current context.

   opts
   `:name` (required) A keyword or string naming the frame.
   `:cross-section` (optional) Cross-section to associate with the
                     current frame.

   Options that specify default values to for future extrusion segments applied to this frame:

   `:cs` (optional) specifies the default number of circular segments to each
         curved extrusion.
   `:curve-radius` (optional) Default curve-radius of future segments applied to this frame."
  schema/frame-schema)

(defop save-transform
  "Save transform by name.

  opts

  `:frame` - The frame to save the current transform of.
  `:name` - Name of the saved transform."
  schema/save-transform-schema)

(defop offset
  "offset cross-sections.

  opts

  `:deta` - number of mm to offset, positive or negative.
  `:join-type` - Type of corners to create when offsetting. Options are :square, :round, or :miter.
  `:simplify` - Remove vertices from resulting contours in cross-section that are less than the
                specified distance epsilon from an imaginary line that passes through its two adjacent vertices."
  schema/offset-schema)

(defop add-ns
  "Namespace frames. If a namespace already exists, it will be be extended (period separated).

  opts

  `:namespace` - string or keyword."
  schema/add-ns-schema)

(defop result
  "Combines frames together with CSG and other 3D geometric operations. Supports operations
  `union`, `difference`, `intersection`, `translate`, `rotate`, `mirror`, `loft`, `hull`,
   and `trim-by-plane`. Frames in result trees are specified by name. Results can also include
   other results (also by name).

  opts

  :name - name of the result.
  :expr - Tree of SCG and other 3D geometric operations."
  schema/result-schema)

(defop union
  "For use in result expressions. Unions frames."
  schema/result-op-schema)

(defop intersection
  "For use in result expressions. Intersects frames."
  schema/result-op-schema)

(defop difference
  "For use in result expressions. Subtracts frames."
  schema/result-op-schema)

#_(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args) schema/import-schema))

(defop insert
  "insert an extrusion at the current position and orientation.

  opts

  `:extrusion` - extrusion to insert.
  `:models` (optional) - models to inserts. Can include results and/or frames. Defaults to singleton vector of last result model.
  `:ns` (optional) - an optional namespace ot associate with inserted models.
  `:end-frame` (optional) - name of a inserted the next segment is relative to. The frame must be defined in the provided extrusion."
  schema/insert-schema)


#_(defn show-coordinate-frame [& args]
    (validate-form `(:plexus.impl/show-coordinate-frame ~@args) schema/show-coordinate-frames-schema))

(defop trim-by-plane
  "For use in result expressions. Trims a model by subracting the halfspace defined by the `:normal`.

  :opts

  `:normal` - [x y z] defines the normal to the plane, points in the direction of the subtracted halfspace.
  `:origin-offset` - Offset of the normal from [0 0 0] in the direction of the normal."
  schema/any-map-schema)

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

(def ^{:doc (-> #'plexus.impl/extrusion? meta :doc) :arglists '([x])} extrusion? impl/extrusion?)
(def ^{:doc (-> #'plexus.impl/model? meta :doc) :arglists '([x])} model? impl/model?)
(def ^{:doc (-> #'plexus.impl/frame? meta :doc) :arglists '([x])} frame? impl/frame?)

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
  ([namespace file-ext]
   (export-models namespace file-ext #".*"))
  ([namespace file-ext match-str]
   (-> (io/file (format "out/%s" file-ext)) (.mkdirs))
   (doall
    (for [[_ x] (ns-map namespace)
          :let [var-meta (meta x)
                var-name (str (:name var-meta))]
          :when (and (:export-model var-meta) (re-matches match-str var-name))]
      (future
        (let [filename (-> var-meta :name name (str (format ".%s" file-ext)))
              m @x]
          (if (m/manifold? m)
            (m/export-mesh (m/get-mesh (cond-> m (= file-ext "3ds") (m/rotate [-90 0 0]))) (format "out/%s/%s" file-ext filename))
            (let [manifold (cond-> (get (:models m) (:main-model m))
                             (#{"3ds"} file-ext) (m/rotate [-90 0 0]))]
              (m/export-mesh (m/get-mesh manifold) (format "out/%s/%s" file-ext filename))))))))))


(defn export
  ([model filename]
   (export model filename (m/material)))
  ([model filename material]
   (m/export-mesh (m/get-mesh (impl/to-manifold (cond-> model (m/cross-section? model) (m/extrude 1/2)))) filename :material material)))
