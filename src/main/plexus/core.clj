(ns plexus.core
  (:refer-clojure :exclude [set])
  (:require
   [plexus.utils :as u]
   [plexus.impl :as impl]
   [scad-clj.model :as m]
   [malli.core :as ma]))

(def curve-schema
  (ma/schema [:map
              [:angle {:optional true} number?]
              [:curve-radius {:optional true} number?]]))

(def linear-extrude-schema
  (ma/schema [:and
              [:map
               [:length {:optional true} number?]
               [:x {:optional true} number?]
               [:y {:optional true} number?]
               [:z {:optional true} number?]]
              [:fn {:error/message "x, y, z, and length are mutually exclusive."}
               (fn [{:keys [x y z length]}]
                 (= 1 (count (remove nil? [x y z length]))))]]))

(def model-schema
  [:map
   [:profile {:optional true} [:sequential any?]]])

(def section-schema
  [:map
   [:name [:or keyword? string?]]
   [:profile {:optional true} [:sequential any?]]])

(def any-map-schema
  (ma/schema [:map]))

(def forward-schema
  [:and
   [:map
    [:x {:optional true} number?]
    [:y {:optional true} number?]
    [:z {:optional true} number?]
    [:length {:optional true} number?]]
   [:fn {:error/message "x, y, z, and length are mutually exclusive."}
    (fn [{:keys [x y z length]}]
      (= 1 (count (remove nil? [x y z length]))))]])

(defn validate-form [form schema]
  (and (impl/parse-args form schema)
       form))

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
  (validate-form `(:plexus.impl/left ~@opts)
                 (ma/schema curve-schema)))

(defn right
    "Extrude an ego-centric right curve. Rotate-extrudes about a y-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/right ~@opts)
                 (ma/schema curve-schema)))

(defn up
    "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric y axis by negative `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/up ~@opts)
                 (ma/schema curve-schema)))

(defn down
  "Extrude an ego-centric right curve. Rotate-extrudes about a x-axis offset
  along the ego-centric x axis by `:curve-radians`.

  opts
  :angle - number of radians to cufve.
  :curve-radius - radius of the curve."
  [& opts]
  (validate-form `(:plexus.impl/down ~@opts)
                 (ma/schema curve-schema)))

(defn arc [& opts]
  (validate-form  `(:plexus.impl/arc ~@opts)
                  (ma/schema curve-schema)))

(defn roll
  "Depricated. Use `(rotate :z rad)` instead.

  Rotates the current transform set by `:angle` radians about the z axis.

  opts
  :angle - Radians of roll"
  [& opts]
  (validate-form `(:plexus.impl/roll ~@opts)
                 (ma/schema curve-schema)))

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
  (validate-form `(:plexus.impl/forward ~@opts) forward-schema))

(defn backward
  "Depreicated. Use `forward` with negative values instead."
  [& opts]
  (validate-form `(:plexus.impl/backward ~@opts) linear-extrude-schema))

(defn hull
  [& opts]
  (validate-form `(:plexus.impl/hull ~@opts) any-map-schema))

(defn model [& args]
  (validate-form `(:plexus.impl/model ~@args) model-schema))

(defn translate [& args]
  (validate-form `(:plexus.impl/translate ~@args)
                 (ma/schema
                  [:and [:map
                         [:x {:optional true} number?]
                         [:y {:optional true} number?]
                         [:z {:optional true} number?]
                         [:global? {:optional true} :boolean]]
                   [:fn {:error/message "Must include x, y, and/or z."}
                    (fn [{:keys [x y z]}]
                      (or x y z))]])))

(defn rotate [& args]
  (validate-form `(:plexus.impl/rotate ~@args)
                 (ma/schema [:map
                             [:x {:optional true} number?]
                             [:y {:optional true} number?]
                             [:z {:optional true} number?]])))

(defn transform [& args]
  (validate-form `(:plexus.impl/transform ~@args)
                 [:map
                  [:transform :transform]]))

(defn spin [& args]
  (validate-form `(:plexus.impl/spin ~@args)
                 any-map-schema))

(defn set [& args]
  (validate-form `(:plexus.impl/set ~@args)
                 any-map-schema))

(defn branch [& args]
  (validate-form `(:plexus.impl/branch ~@args)
                 (ma/schema [:map
                             [:from [:or keyword? string?]]
                             [:with {:optional true} [:vector [:or keyword? string?]]]])))

(defn segment [& args]
  (validate-form `(:plexus.impl/segment ~@args) any-map-schema))

(defn to [& p]
  (let [[opts parsed-path] (impl/parse-path p)
        extrude* (map (fn [[x & xs]]
                     (list* x :to (:models opts) xs))
                   parsed-path)]
    (segment extrude*)))

(defn mask [& args]
  (validate-form `(:plexus.impl/model ~@(conj (vec args) :mask? true))
                 model-schema))

(defn frame [& args]
  (validate-form `(:plexus.impl/model ~@(concat args [:mask? false]))
                 section-schema))

(defn save-transform [& args]
  (validate-form `(:plexus.impl/save-transform ~@args)
                 [:map
                  [:name [:or keyword? string?]]
                  [:frame [:or keyword? string?]]]))

(defn offset [& args]
  (validate-form `(:plexus.impl/offset ~@args)
                 [:map
                  [:offset number?]]))

(defn minkowski [& args]
  (validate-form `(:plexus.impl/minkowski ~@args)
                 any-map-schema))

(defn add-ns [& args]
  (validate-form `(:plexus.impl/add-ns ~@args)
                 [:map
                  [:namespace [:or keyword? string?]]]))

(defn forward-until [& args]
  (validate-form `(:plexus.impl/forward-until ~@args)
                 any-map-schema))

(defn ignore [& args]
  (validate-form `(:plexus.impl/ignore ~@args)
                 any-map-schema))

(defn result [& args]
  (validate-form `(:plexus.impl/result ~@args)
                 [:map
                  [:name [:or keyword? string?]]
                  [:expr [:or keyword? [:sequential any?]]]]))


(def result-op-schema
  [:sequential [:or keyword? string? sequential?]])

(defn union [& args]
  (ma/coerce result-op-schema `(:plexus.impl/union ~@args) nil {:registry u/registry}))

(defn intersection [& args]
  (ma/coerce result-op-schema `(:plexus.impl/intersection ~@args) nil {:registry u/registry}))

(defn difference [& args]
  (ma/coerce result-op-schema `(:plexus.impl/difference ~@args) nil {:registry u/registry}))

(defn slice [& args]
  (validate-form `(:plexus.impl/slice ~@args)
                 [:map
                  [:length number?]]))

(defn iso-hull [& args]
  (validate-form `(:plexus.impl/iso-hull ~@args)
                 [:map
                  [:n-segments :pos-int]]))

(defn import [& args]
  (validate-form `(:plexus.impl/import ~@args)
                 [:map
                  [:stl string?]]))

(defn show-coordinate-frame [& args]
  (validate-form `(:plexus.impl/show-coordinate-frame ~@args)
                 [:map
                  [:radius {:optional true} number?]
                  [:length {:optional true} number?]
                  [:label {:optional true} string?]
                  [:frame-offset {:optional true} [:tuple int? int? int?]]]))

(defn subtract [a & args]
  (let [all-modules (reduce (fn [ret arg]
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
  (let [all-modules (reduce (fn [ret arg]
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
  (let [all-modules (reduce (fn [ret arg]
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

(defn extrude [& extrude-forms]
  (impl/extrude extrude-forms))

(defmacro points
  [& forms]
  (let [[opts extrude*] (impl/parse-path forms)
        axes (or (:axes opts) [:x :y])
        meta-props (:meta-props opts)
        sym (gensym "tv-")
        clauses (for [axis axes]
                  (case axis
                    :x `(nth ~sym 0)
                    :y `(nth ~sym 1)
                    :z `(nth ~sym 2)))]
    `(let [p# (impl/extrude* (assoc impl/default-state :name ~(str name)) ~extrude*)
           m# (meta p#)]
       (with-meta
         (impl/path-points p# (fn [~sym]
                                (vector ~@clauses)) ~meta-props)
         m#))))

(defmacro defmodel [name & extrude*]
  (let [[opts extrude*] (impl/parse-path extrude*)]
    `(binding [m/*fn* ~(get opts :fn 10)]
       (def ~name
         (impl/extrude* (assoc impl/default-state
                               :name ~(str name)
                               :namespace ~(str *ns* "." name))
                        ~extrude*)))))

(defn lookup-transform
  [model name]
  (or (-> model meta :segment-groups name peek meta :end-transform)
      (-> model meta :transforms name)))

(defn lookup-property
  [model name property]
  (-> model meta :segment-groups name peek meta property))
