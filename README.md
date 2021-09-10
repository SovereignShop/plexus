# scad-paths

An [scad-clj](https://github.com/farrellm/scad-clj) paths library. It defines a simple, extensible language for extruding shapes in a step-wise, relative fashion using egocentric reference frames.

You specify 3D models by providing a series of transformations to an outer shape and inner shape.

# Examples

In the following example, our outer shape is a circle with radius of 6. The inner
shape is a circle of radius 4. We then specify a series of transformations to then
outer an inner shape. 

``` clojure
(require '[scad-clj.scad :as s]
         '[scad-clj.model :as m]
         '[scad-paths.core :refer [context left right forward up down path ->main-model]]

(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            [(left :angle (/ Math/PI 2)) (left :angle (/ Math/PI 2))]
            [(right :angle (/ Math/PI 2)) (right :angle (/ Math/PI 2))]
            [(forward :length 10) (forward :length 10)]
            [(up) (up)]])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))
```

Obviously, there is a lot of code duplication here. After providing the shape for the inner and outer forms,
the transformations we apply to each are equivalent. We can get rid of that duplication by only providing one 
transformation that is applied to both the outer and inner context:

``` clojure    
(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (left :angle (/ Math/PI 2))
            (right :angle (/ Math/PI 2))
            (forward :length 10)
            (up)])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))
```

![Pipe Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/pipe-example.png)


This path is equivalent to the one above.

## Hulls

Hulls are often a great way to transform between shapes using openscad. Hulls in scad-paths
are applied in a stack-like fashion to the previous two shapes:

``` clojure
(->> (path {:curve-radius 20 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (forward :length 20)
            [(context :shape (m/square 20 20)) (context :shape (m/square 16 16))]
            (forward :length 20)
            (hull)
            (forward :length 20)
            [(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            (forward :length 20)
            (hull)])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))
```

![Hull Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/hull-example.png)

i.e. it pops the previous two segments off, hulls them, then pushes the result back onto the stack. You can specify the parameter `n-segments` if you'd like to hull between several segments.

## Branching

You can specify branches with a special form:

``` clojure
(->> (path {:curve-radius 10 :fn 70}
           [[(context :shape (m/circle 6)) (context :shape (m/circle 4))]
            [:branch
             [(left) (right) (forward :length 20)]]
            [:branch
             [(right) (left) (forward :length 20)]]])
     (->main-model)
     (s/write-scad)
     (spit "test.scad"))
```

![Branching Example](https://github.com/SovereignShop/scad-paths/blob/main/resources/images/branching-example.png)


The body of the branch is just another path.

# Extensions

Path segments are handled by a multi-method called `path-segment` in the core namespace. You can easily extend it with your own custom segments. You simply
have to ensure that the position and orientation are updated to correspond 
to the end of your shape, where the next segment should continue. See the
core namespace for examples.
