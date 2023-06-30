# Plexus

Plexus defines a simple, extensible language for defining polygons and extruding them in a piecewise fashion using egocentric reference frames.

You can specify 3D models by providing a series of extrusions to a set of cross sections, which are then composited using CSG operations.



# Status

Pre-alpha, syntax and internals likely to change.
 
# Examples

In the following example, our outer profile is a circle with radius of 6. The mask
profile is a circle of radius 4. We then specify a series of egocentric transformations to the
outer and inner cross sections. 

``` clojure
(require
 '[clj-manifold3d.core :as m]
 '[plexus.core
   :refer [result frame left right forward up down hull extrude set branch arc defmodel
           rotate translate segment difference union intersection points export insert
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

Obviously there is a lot of code duplication here. After providing the profile for the inner and outer forms,
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
We can elide this, as by default each segement will reply to every frame you have defined:

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
     (up :angle (/ Math/PI 2) :to [:body :mask]))
    (export "pipes.glb"))
```

This extrude is equivalent to the one above.

## Hulls

Hulls are often a great way to transform between cross sections. Hulls in plexus
are applied in a stack-like fashion to the previous two cross sections:

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

i.e. it pops the previous two segments off, hulls them, then pushes the result back onto the stack. You can specify the parameter `n-segments` if you'd like to hull between several segments.

## Lofts 

You can loft between a sequence of isomorphic cross-sections with `loft`. Edges are constructed between corresponding
vertices of each cross-section.

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


The body of the branch is just another extrude. Notice the mask is not subtracted from the frame until the full tree is constructed.

## Gaps

You can make any segment a gap with the gap parameter:

``` clojure
(-> (extrude
     (frame :cross-section (m/circle 6) :name :body :curve-radius 10)
     (for [i (range 3)]
       [(left :angle (/ Math/PI 2) :gap true)
        (right :angle (/ Math/PI 2))]))
    (export "gaps.glb"))
```

![Gap Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/gap-example.png)

Notice notice the extrude forms automatically flattened. You can arbitrarily nest loops.

## Composition

The easiest way to compose extrusions is with `insert`. 

``` clojure
(let [pipe (extrude
            (frame :cross-section (m/circle 6) :name :outer :curve-radius 10)
            (frame :cross-section (m/circle 4) :name :inner)
            (forward :length 30))]
  (-> (extrude
       (result :name :pipes
               :expr (trim-by-plane {:normal [-1 0 0]} (difference :pipe/outer :pipe/inner)))
       (frame :name :origin)
       (translate :z 5)

       (for [i (range 4)]
         (branch
          :from :origin
          (rotate :x (* i 1/2 Math/PI))
          (insert :extrusion pipe
                  :models [:outer :inner]
                  :ns :pipe
                  :end-frame :outer))))
      (export "insert.glb")))
```

![Segment Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/insert-example.png)

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
