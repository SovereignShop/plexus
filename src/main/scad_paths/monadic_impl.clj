(ns scad-paths.monadic-impl
  (:require
   [scad-paths.core :as c]))

(-> default-state
    (body :shape (m/circle 10) :name :body)
    (forward :length 20)
    (result :name :tmp :expr :body))
