# Plexus

Plexus defines a simple, extensible language for defining polygons and extruding them in a piecewise fashion using egocentric reference frames.

You can specify 3D models by providing a series of extrusions to a set of profiles, which are then composited using CSG operations.

# Status

Pre-alpha, syntax and internals likely to change.
 
# Examples

In the following example, our outer profile is a circle with radius of 6. The mask
profile is a circle of radius 4. We then specify a series of egocentric transformations to the
outer and inner profiles. 

``` clojure
(require '[scad-clj.scad :as s]
         '[scad-clj.model :as m]
         '[plexus.core :refer [frame left right forward up down hull extrude set branch]]

(->> (extrude
      (result :name :pipes 
              :expr (difference :outer :inner))
      
      (frame :profile (m/circle 6) :name :outer)
      (frame :profile (m/circle 4) :name :inner)
      (set :curve-radius 20 :fn 70 :to [:outer]) (set :curve-radius 20 :fn 70 :to [:inner])

      (left :angle (/ Math/PI 2) :to [:outer]) (left :angle (/ Math/PI 2) :to [:inner])
      (right :angle (/ Math/PI 2) :to [:outer]) (right :angle (/ Math/PI 2) :to [:inner])
      (forward :length 10 :to [:outer]) (forward :length 10 :to [:inner])
      (up :to [:outer]) (up :to [:inner]))
     (s/write-scad)
     (spit "test.scad"))
```

Obviously there is a lot of code duplication here. After providing the profile for the inner and outer forms,
the transformations we apply to each are equivalent. We can get rid of that duplication by only providing one 
transforming both profiles with each segment:

``` clojure
(->> (extrude 
      (result :name :pipes
              :expr (difference :outer :inner))
                  
      (frame :profile (m/circle 6) :name :outer)
      (frame :profile (m/circle 4) :name :inner)
      (set :curve-radius 20 :fn 70 :to [:outer :inner])

      (left :angle (/ Math/PI 2) :to [:outer :inner])
      (right :angle (/ Math/PI 2) :to [:outer :inner])
      (forward :length 10 :to [:outer :inner])
      (up :to [:outer :inner])
      (forward :length 20 :to [:outer :inner]))
     (s/write-scad)
     (spit "test.scad"))
```

![Pipe Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/pipe-example.png)


This is equivalent to the one above, but we can still see there is a lot of duplication. The `:to [:outer :inner]` is repeated in each segment.
We can elide this, as by default each segement will reply to every frame you have defined:

``` clojure
(->> (extrude
      (result :name :pipes
              :expr (difference :outer :inner))
                   
      (frame :profile (m/circle 6) :name :outer)
      (frame :profile (m/circle 4) :name :inner)
      (set :curve-radius 20 :fn 70)

      (left :angle (/ Math/PI 2))
      (right :angle (/ Math/PI 2))
      (forward :length 10)
      (up)
      (forward :length 20))
     (s/write-scad)
     (spit "test.scad"))
```

This extrude is equivalent to the one above.

## Hulls

Hulls are often a great way to transform between profiles using openscad. Hulls in plexus
are applied in a stack-like fashion to the previous two profiles:

``` clojure
(->> (extrude 
      (result :name :pipes
              :expr (difference :outer :inner))
                   
      (frame :profile (m/circle 6) :name :outer)
      (frame :profile (m/circle 4) :name :inner)
      (set :curve-radius 20 :fn 70)

      (forward :length 20)

      (set :profile (m/square 20 20) :to [:outer])
      (set :profile (m/square 16 16) :to [:inner])

      (forward :length 20)
      (hull)
      (forward :length 20)

      (set :profile (m/circle 6) :to [:outer])
      (set :profile (m/circle 4) :to [:inner])

      (forward :length 20)
      (hull))
     (s/write-scad)
     (spit "test.scad"))
```

![Hull Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/hull-example.png)

i.e. it pops the previous two segments off, hulls them, then pushes the result back onto the stack. You can specify the parameter `n-segments` if you'd like to hull between several segments.

## Branching

Branches work as you'd expect:

``` clojure    
(->> (extrude 
      (result :name :pipes
              :expr (difference :outer :inner))
              
      (frame :profile (m/circle 6) :name :outer)
      (frame :profile (m/circle 4) :name :inner)
      (set :curve-radius 10 :fn 70)

      (branch :from :outer (left) (right) (forward :length 20))
      (branch :from :outer (right) (left) (forward :length 20)))
     (s/write-scad)
     (spit "test.scad"))
```

![Branching Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/branching-example.png)


The frame of the branch is just another extrude. Notice the mask is not subtracted from the frame until the full tree is constructed.

## Gaps

You can make any segment a gap with the gap parameter:

``` clojure
(->> (extrude
      (result :name :pipes
              :expr (difference :outer :inner))
      (frame :profile (m/circle 6) :name :outer :curve-radius 10 :fn 70)
      (left :angle (/ Math/PI 2) :gap true)
      (right :angle (/ Math/PI 2))
      (left :gap true)
      (right)
      (left :gap true)
      (right))
     (s/write-scad)
     (spit "test.scad"))

```


![Gap Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/gap-example.png)

## Segments

`segment` is kind of like a `do` in clojure. Importantly, they enable you to use loops in your extrude definition.

``` clojure
(->> (extrude
      (result :name :pipes
              :expr (difference :outer :inner))
      (frame :profile (m/circle 6) :name :outer :curve-radius 10 :fn 70)
      (frame :profile (m/circle 4) :name :inner)
      (segment
       (for [i (range 4)]
         (branch
          :from :outer
          (rotate :x (* i 1/2 Math/PI))
          (forward :length 30)))))
    (s/write-scad)
    (spit "test.scad"))
```

![Segment Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/segment-example.png)

Also use `segment` to nest paths.

``` clojure
(let [pipe-path (extrude
                 (frame :profile (m/circle 6) :name :outer :curve-radius 10 :fn 70)
                 (frame :profile (m/circle 4) :name :inner)
                 (forward :length 30))]
  (->> (extrude
        (result :name :pipes
                :expr (difference :outer :inner))
        (frame :name :origin)

        (segment
         (for [i (range 4)]
           (branch
            :from :origin
            (rotate :x (* i 1/2 Math/PI))
            (segment pipe-path)))))
            
        (s/write-scad)
        (spit "test.scad")))
```

This produces equivalent output to above. Notice the nested pipes-path inherits the frame in which it's placed.

## Points

`points` is similar to `extrude` except you use it to define 2D polygons. Here's an example of how to define a circle.

``` clojure
(->> (m/polygon
      (points
       :axes [:x :z]
       (frame :name :origin :fn 20)
       (translate :x 50)
       (left :angle (* 2 Math/PI) :curve-radius 50)))
     (s/write-scad)
     (spit "test.scad"))
```

![Points Example](https://github.com/SovereignShop/plexus/blob/main/resources/images/points-example.png)


# Extensions

Path segments are handled by a multi-method called `path-form` in the core namespace. You can easily extend it with your own custom segments. You simply have to ensure that the position and orientation are updated to correspond to the end of your shape, where the next segment should continue. See the
core namespace for examples.
