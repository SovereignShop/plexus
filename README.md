[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.cartesiantheatrics/plexus.svg)](https://clojars.org/org.clojars.cartesiantheatrics/plexus)

# Plexus

Plexus is a simple tool for defining cross sections and extruding them in a piece-wise fashion using egocentric reference frames.

You can specify 3D models by providing a series of extrusions to a set of cross sections, which are then composited using CSG operations.

# Install

This library uses [java bindings](https://github.com/SovereignShop/manifold) to the native library Manifold. You need to include the correct bindings for your platform. For linux, I recommend using `linux-x86_64` or `linux-cuda-x86_64` classifiers. For example:

```clojure
{:deps {org.clojars.cartesiantheatrics/manifold3d$linux-cuda-x86_64 {:mvn/version "1.0.64"}}}
```

The library will fall back to CPU implementations if cuda is not found on the system, but the cuda jar is much larger at about 15mb vs. 2mb. There are experimental TBB (Threading Building Blocks) and OMP (OpenMP) flavors for the linux bindings using classifiers `linux-TBB-x86_64`/`linux-OMP-x86_64`. 

```clojure
{:deps {org.clojars.cartesiantheatrics/manifold3d$linux-TBB-x86_64 {:mvn/version "1.0.64"}}}
```

There are also bindings for Mac (currently only TBB) using the classifiers `mac-x86_64` or `mac-TBB-x86_64`.

There are no windows jars available in a maven repository currently. See the github build artifacts for an experimental windows jar. The windows build unfortunately requires the dreaded `CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=TRUE`

You also need [assimp](https://github.com/assimp/assimp) available on you system.

``` sh
sudo apt install libassimp-dev
```

# Status

Alpha, syntax and internals may change.

# Examples

In the following example, our outer cross section is a circle with radius of 6. The mask
cross section is a circle of radius 4. We then specify a series of egocentric transformations to the
outer and inner cross sections. 

``` clojure
(require
 '[clj-manifold3d.core :as m]
 '[plexus.core
   :refer [result frame left right forward up down hull extrude set branch
           rotate translate difference union intersection points export insert
           loft trim-by-plane offset]])

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20 :to [:body]) (set :curve-radius 20 :to [:mask])

     (left :angle (/ Math/PI 2) :to [:body])
     (left :angle (/ Math/PI 2) :to [:mask])

     (right :angle (/ Math/PI 2) :to [:body])
     (right :angle (/ Math/PI 2) :to [:mask])

     (forward :length 10 :to [:body])
     (forward :length 10 :to [:mask])

     (up :angle (/ Math/PI 2) :to [:body])
     (up :angle (/ Math/PI 2) :to [:mask]))
    (export "test.glb"))
```

Obviously there is a lot of code duplication here. After providing the cross section for the inner and outer forms,
the transformations we apply to each are equivalent. We can get rid of that duplication by only providing one 
transforming both cross sections with each segment:

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20 :to [:body :mask])

     (left :angle (/ Math/PI 2) :to [:body :mask])
     (right :angle (/ Math/PI 2) :to [:body :mask])
     (forward :length 10 :to [:body :mask])
     (up :angle (/ Math/PI 2) :to [:body :mask]))
    (export "pipes.glb"))
```

![Pipe Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/pipe-example.png)


This is equivalent to the one above, but we can still see there is a lot of duplication. The `:to [:outer :inner]` is repeated in each segment.
We can elide this, as by default each segment will reply to every frame you have defined:

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20)

     (left :angle (/ Math/PI 2))
     (right :angle (/ Math/PI 2))
     (forward :length 10)
     (up :angle (/ Math/PI 2)))
    (export "pipes.glb"))
```

This extrude is equivalent to the one above.

## Hulls

Hulls are often a great way to transform between cross sections. You can wrap any sequence of extrusions
in a hull form to make a convex hull out of those segments:

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 20)
     (hull
      (hull
       (forward :length 20)
       (set :cross-section (m/square 20 20 true) :to [:body])
       (set :cross-section (m/square 16 16 true) :to [:mask])
       (forward :length 20))
      (set :cross-section (m/circle 6) :to [:body])
      (set :cross-section (m/circle 4) :to [:mask])
      (forward :length 20)))
    (export "hull.glb"))
```

![Hull Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/hull-example.png)

## Lofts

You can loft between a sequence of isomorphic cross-sections with `loft`. Edges are constructed between corresponding vertices of each cross-section.

``` clojure
(-> (extrude
     (result :name :pipes :expr :body)
     (frame :cross-section (m/difference (m/circle 20) (m/circle 18)) :name :body)
     (loft
      (forward :length 1)
      (for [i (range 3)]
        [(translate :x 8)
         (forward :length 20)
         (translate :x -8)
         (forward :length 20)])))
    (export "loft.glb"))
```

![Loft Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/loft-example.png)

## Branching

Branches work as you'd expect.

``` clojure
(def pi|2 (/ Math/PI 2))

(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 10)

     (branch :from :body (left :angle pi|2) (right :angle pi|2) (forward :length 20))
     (branch :from :body (right :angle pi|2) (left :angle pi|2) (forward :length 20)))
    (export "branch.glb"))
```

![Branching Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/branching-example.png)

The body of the branch is just another extrude. The required ":from" property determines the starting coordinate frame of the branch. There's also an optional `:with` parameter that specifies which frames to include in the branch.

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :cross-section (m/circle 6) :name :body)
     (frame :cross-section (m/circle 4) :name :mask)
     (set :curve-radius 10)

     (branch :from :body (left :angle pi|2) (right :angle pi|2) (forward :length 20))
     (branch
      :from :body
      :with [:body]
      (right :angle pi|2)
      (left :angle pi|2)
      (forward :length 20)))
    (export "branch-with.glb" (m/material :color [0. 0.7 0.7 1.0] :metalness 0.2)))
```

![Branching Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/branching-with-example.png)

Note that you can introduce new frames at any point. The starting transform of any new frame is inherited from the previously introduced frame.

## Gaps

You can make any segment a gap with the `:gap` parameter:

``` clojure
(-> (extrude
     (frame :cross-section (m/circle 6) :name :body :curve-radius 10)
     (for [i (range 3)]
       [(left :angle (/ Math/PI 2) :gap true)
        (right :angle (/ Math/PI 2))]))
    (export "gaps.glb"))
```

![Gap Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/gap-example.png)

You can also specify which subset of active frames should be a gap by supplying a vector frame names.

``` clojure
(-> (extrude
     (frame :cross-section (m/circle 6) :name :body :curve-radius 10)
     (for [i (range 3)]
       [(left :angle (/ Math/PI 2) :gap [:body])
        (right :angle (/ Math/PI 2))]))
    (export "gaps.glb"))
```

This is equivalent to above. `:gap true` is equivalent to gapping all active frames.

## Insert

The easiest way to compose extrusions is with `insert`. 

``` clojure
(let [pipe (extrude
            (frame :cross-section (m/circle 6) :name :outer :curve-radius 10)
            (frame :cross-section (m/circle 4) :name :inner)
            (forward :length 30))]
  (-> (extrude
       (result :name :pipes
               :expr (->> (difference :pipe/outer :pipe/inner)
                          (trim-by-plane :normal [-1 0 0])
                          (translate :z 30)))
       (frame :name :origin)

       (for [i (range 4)]
         (branch
          :from :origin
          (rotate :x (* i 1/2 Math/PI))
          (insert :extrusion pipe
                  :models [:outer :inner]
                  :ns :pipe
                  :end-frame :outer))))
      (export "insert.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2))))
```

![Insert Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/insert-example.png)

Here we're inserting the models `:outer` and `:inner` at four different locations. We're also namespacing the inserted models with `:pipe`. `:end-frame` specifies the frame to continue on in the next segment.

## Translate 

You can "move" without extruding using `translate`.

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :name :body :cross-section (m/circle 6))
     (frame :name :mask :cross-section (m/circle 4))

     (forward :length 10)
     (translate :x 5 :z 10)
     (forward :length 10))
    (export "translate.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))
```

![Translate Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/translate-example.png)

## Rotate 

You can rotate in place without extruding using `rotate`.

``` clojure
(-> (extrude
     (result :name :pipes
             :expr (difference :body :mask))

     (frame :name :body :cross-section (m/circle 6))
     (frame :name :mask :cross-section (m/circle 4))

     (forward :length 15)
     (rotate :x (/ Math/PI 2))
     (forward :length 15))
    (export "rotate.glb" (m/material :color [0 0.7 0.7 1.0] :metalness 0.2)))
```

![Rotate Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/rotate-example.png)

## Result

Result expressions have been demonstrated in every example so far. As you can see, they represent an arbitrarily nested CSG expression. Result expressions can reference other results by name. the set of operations that can be appear in result expressions are: `union`, `difference`, `intersection`, `hull`, `translate`, `rotate`, `mirror`, and `trim-by-plane`.

## Points

`points` is similar to `extrude` except you use it to define 2D polygons. Here's an example of how to define a circle.

``` clojure
(-> (m/cross-section
     (points
      :axes [:x :z]
      (frame :name :origin)
      (translate :x 50)
      (left :angle (* 2 Math/PI) :curve-radius 50 :cs 20)))
    (m/extrude 1)
    (export "circle.glb"))
```

![Points Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/points-example.png)

## Using Extrusions

You can access extrusion models using `get-model`.

``` clojure
(m/difference (get-model extrusion :body) 
              (get-model extrusion :mask))
```

