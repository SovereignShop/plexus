(ns plexus.schema
  (:require
   [clj-manifold3d.core :as m]
   [malli.core :as ma]
   [plexus.impl :as impl]))

(def curve-schema
  [:map
   [:angle {:optional true} number?]
   [:curve-radius {:optional true} number?]
   [:transform-step-fn {:optional true} fn?]])

(def curve-generic-schema
  [:map
   [:angle {:optional true} number?]
   [:curve-radius {:optional true} number?]
   [:direction {:optional true} [:enum :left :right :up :down]]
   [:roll {:optional true} number?]])

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

(def insert-schema
  [:map
   [:extrusion :extrusion]
   [:models {:optional true} [:sequential keyword?]]
   [:ns {:optional true} simple-keyword?]
   [:end-frame {:optional true} keyword?]])

(def frame-schema
  [:map
   [:name [:or keyword? string?]]
   [:cross-section {:optional true} :cross-section]
   [:cs {:optional true} int?]])

(def any-map-schema
  [:map])

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

(def translate-schema
  (ma/schema
   [:and [:map
          [:x {:optional true} number?]
          [:y {:optional true} number?]
          [:z {:optional true} number?]
          [:global? {:optional true} :boolean]]
    [:fn {:error/message "Must include x, y, and/or z."}
     (fn [{:keys [x y z]}]
       (or x y z))]]))

(def mirror-schema
  (ma/schema
   [:map
    [:normal [:tuple {:title "tmp"} number? number? number?]]]))

(def rotate-schema
  [:map
   [:x {:optional true} number?]
   [:y {:optional true} number?]
   [:z {:optional true} number?]])

(def transform-schema
  [:map
   [:replace :transform]])

(def branch-schema
  [:map
   [:from [:or keyword? string?]]
   [:with {:optional true} [:vector [:or keyword? string?]]]])

(def save-transform-schema
  [:map
   [:name [:or keyword? string?]]
   [:frame [:or keyword? string?]]])

(def offset-schema
  [:map
   [:delta number?]
   [:simplify {:optional true} number?]
   [:join-type {:optional true} [:or [:= :square] [:= :round] [:= :miter]]]])

(def add-ns-schema
  [:map
   [:namespace [:or keyword? string?]]])

(def result-op-schema
  [:sequential [:or keyword? string? sequential? map?]])

(def result-schema
  [:map
   [:name [:or keyword? string?]]
   [:expr any? #_[:or keyword? [:sequential any?]]]])

(def hull-schema
  [:map
   [:n-segments :pos-int]])

(def import-schema
  [:map
   [:stl string?]])

(def show-coordinate-frames-schema
  [:map
   [:radius {:optional true} number?]
   [:length {:optional true} number?]
   [:label {:optional true} string?]
   [:frame-offset {:optional true} [:tuple int? int? int?]]])

(defn validate-form [form schema]
  (impl/parse-args form schema))
