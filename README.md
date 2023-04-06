# Plexus

Plexus defines a simple, extensible language for extruding shapes in a step-wise, relative fashion using egocentric reference frames.

You can specify 3D models by providing a series of extrusions to a set of shapes and masks, which are then composited in a controlled
order using CSG operations.

# Status

Pre-alpha, syntax and internals likely to change.
 
# Examples

In the following example, our outer shape is a circle with radius of 6. The mask
shape is a circle of radius 4. We then specify a series of egocentric transformations to the
outer and inner shapes. 

``` clojure
(require '[scad-clj.scad :as s]
         '[scad-clj.model :as m]
         '[plexus.core :refer [model left right forward up down hull path set branch]]

(->> (path
      [(model :shape (m/circle 6) :mask? false :name :body)
       (model :shape (m/circle 4) :mask? true :name :mask)
       (set :curve-radius 20 :fn 70 :to [:body]) (set :curve-radius 20 :fn 70 :to [:mask])

       (left :angle (/ Math/PI 2) :to [:body]) (left :angle (/ Math/PI 2) :to [:mask])
       (right :angle (/ Math/PI 2) :to [:body]) (right :angle (/ Math/PI 2) :to [:mask])
       (forward :length 10 :to [:body]) (forward :length 10 :to [:mask])
       (up :to [:body]) (up :to [:mask])])
     (s/write-scad)
     (spit "test.scad"))
```

Obviously there is a lot of code duplication here. After providing the shape for the inner and outer forms,
the transformations we apply to each are equivalent. We can get rid of that duplication by only providing one 
transforming both shapes with each segment:

``` clojure
(->> (path [(model :shape (m/circle 6) :mask? false :name :body)
            (model :shape (m/circle 4) :mask? true :name :mask)
            (set :curve-radius 20 :fn 70 :to [:body :mask])

            (left :angle (/ Math/PI 2) :to [:body :mask])
            (right :angle (/ Math/PI 2) :to [:body :mask])
            (forward :length 10 :to [:body :mask])
            (up :to [:body :mask])
            (forward :length 20 :to [:body :mask])])
     (s/write-scad)
     (spit "test.scad"))
```

![Pipe Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/pipe-example.png)


This is equivalent to the one above, but we can still see there is a lot of duplication. The `:to [:body :mask]` is repeated in each segment.
We can elide this, as by default each segement will reply to every model you have defined:

``` clojure
(->> (path [(model :shape (m/circle 6) :mask? false :name :body)
            (model :shape (m/circle 4) :mask? true :name :mask)
            (set :curve-radius 20 :fn 70)

            (left :angle (/ Math/PI 2))
            (right :angle (/ Math/PI 2))
            (forward :length 10)
            (up :to [:body :mask])
            (forward :length 20)])
     (s/write-scad)
     (spit "test.scad"))
```

This path is equivalent to the one above.

## Hulls

Hulls are often a great way to transform between shapes using openscad. Hulls in plexus
are applied in a stack-like fashion to the previous two shapes:

``` clojure
(->> (path [(model :shape (m/circle 6) :mask? false :name :body)
            (model :shape (m/circle 4) :mask? true :name :mask)
            (set :curve-radius 20 :fn 70)

            (forward :length 20)

            (set :shape (m/square 20 20) :to [:body])
            (set :shape (m/square 16 16) :to [:mask])

            (forward :length 20)
            (hull)
            (forward :length 20)

            (set :shape (m/circle 6) :to [:body])
            (set :shape (m/circle 4) :to [:mask])

            (forward :length 20)
            (hull)])
     (s/write-scad)
     (spit "test.scad"))
```

![Hull Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/hull-example.png)

i.e. it pops the previous two segments off, hulls them, then pushes the result back onto the stack. You can specify the parameter `n-segments` if you'd like to hull between several segments.

## Branching

Branches work as you'd expect:

``` clojure    
(->> (path [(model :shape (m/circle 6) :mask? false :name :body)
            (model :shape (m/circle 4) :mask? true :name :mask)
            (set :curve-radius 10 :fn 70)

            (branch (left) (right) (forward :length 20))
            (branch (right) (left) (forward :length 20))])
     (s/write-scad)
     (spit "test.scad"))
```

![Branching Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/branching-example.png)


The body of the branch is just another path. Notice the mask is not subtracted from the body until the full tree is constructed.

## Gaps

You can make any segment a gap with the gap parameter:

``` clojure
(->> (path [(model :shape (m/circle 6) :mask? false :name :body :curve-radius 10 :fn 70)
            (left :angle (/ Math/PI 2) :gap true)
            (right :angle (/ Math/PI 2))
            (left :gap true)
            (right)
            (left :gap true)
            (right)])
     (s/write-scad)
     (spit "test.scad"))

```

![Gap Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/gap-example.png)


# Extensions

Path segments are handled by a multi-method called `path-form` in the core namespace. You can easily extend it with your own custom segments. You simply have to ensure that the position and orientation are updated to correspond to the end of your shape, where the next segment should continue. See the
core namespace for examples.
