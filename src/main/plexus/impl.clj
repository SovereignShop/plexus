(ns plexus.impl
  (:refer-clojure :exclude [set])
  (:import [manifold3d.glm DoubleMat4x3 MatrixTransforms DoubleVec3])
  (:require
   [clojure.string :as string]
   [clojure.core.matrix :as mat]
   [clojure.walk :refer [postwalk]]
   [plexus.segment :as sg]
   [plexus.transforms :as tf]
   [plexus.triangles :as triangles]
   [plexus.utils :as u]
   [clj-manifold3d.core :as man]
   [scad-clj.model :as m]
   [camel-snake-kebab.core :as csk]
   [malli.core :as ma]))

(defn parse-args
  ([form]
   (parse-args form nil))
  ([form schema]
   (loop [[arg & args] (next form)
          kvs (transient {:op (first form)})]
     (if (nil? arg)
       (let [ret (persistent! kvs)]
         (if schema
           (ma/coerce schema ret nil {:registry u/registry})
           ret))
       (if (keyword? arg)
         (recur (next args)
                (assoc! kvs arg (first args)))
         (let [ret (persistent! (assoc! kvs ::list (vec (cons arg args))))]
           (if schema
             (ma/coerce schema ret nil {:registry u/registry})
             ret)))))))

(defn parse-path [path-spec]
  (loop [[x & xs] path-spec
         args {}]
    (cond (nil? x)
          [args nil]

          (keyword? x)
          (recur (next xs) (assoc args x (first xs)))

          :else
          [args (vec (cons x xs))])))
