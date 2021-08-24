(ns scad-paths.utils
  (:require
   [clojure.string :as string]
   [scad-clj.model :as m]
   [scad-clj.scad :as s]))

(defn half [x] (/ x 2))
(def pi Math/PI)
(def hpi (/ Math/PI 2))
(def qpi (/ Math/PI 4))
(def opi (/ Math/PI 8))

(defn semi-circle [r angle]
  (let [n-steps (or m/*fn* 10)
        step-size (/ angle n-steps)]
    (m/polygon
     (cons [0 0]
           (for [step (range (inc n-steps))]
             (let [d (* step step-size)]
               [(* r (Math/cos d)) (* r (Math/sin d))]))))))

(defn cone [top-radius bottom-radius height]
  (m/hull (m/cylinder bottom-radius 1.0 :center false)
          (->> (m/cylinder top-radius 1 :center false)
               (m/translate [0 0 (- height 1)]))))

(defn pyramid
  ([top-size bottom-size height]
   (pyramid top-size bottom-size height 1))
  ([top-size bottom-size height bottom-thickness]
   (m/hull (->> (m/square top-size top-size)
                (m/extrude-linear {:height bottom-thickness :center false}))
           (->> (m/square bottom-size bottom-size)
                (m/extrude-linear {:height bottom-thickness :center false})
                (m/translate [0 0 (- height bottom-thickness)])))))

(defn square-circle-hull
  ([bottom-size top-radius height]
   (square-circle-hull bottom-size top-radius height 1))
  ([bottom-size top-radius height bottom-thickness]
   (m/hull
    (->> (m/circle top-radius)
         (m/extrude-linear {:height bottom-thickness :center false})
         (m/translate [0 0 (- height bottom-thickness)]))
    (->> (m/square bottom-size bottom-size)
         (m/extrude-linear {:height bottom-thickness :center false})))))

(defn circle-square-hull
  ([bottom-size top-radius height]
   (square-circle-hull bottom-size top-radius height 1))
  ([bottom-size top-radius height bottom-thickness]
   (m/hull (->> (m/square bottom-size bottom-size)
                (m/extrude-linear {:height bottom-thickness :center false})
                (m/translate [0 0 (- height bottom-thickness)]))
           (->> (m/circle top-radius)
                (m/extrude-linear {:height bottom-thickness :center false})))))

(defn helix
  [shape steps height twist dir]
  (let [shape-3d (->> shape (m/extrude-linear {:height 0.1}))
        section (m/hull
                 (for [step (range 2)]
                   (->> shape-3d
                        (m/translate [0 0 (* step  (/ height steps))])
                        (m/rotatec [0 0 (* step (case dir :clockwise 1 -1) (/ twist steps))]))))]
    (m/union
     section
     (for [step (range 1 steps)]
       (->> section
            (m/translate [0 0 (* step (/ height steps))])
            (m/rotatec [0 0 (* step (case dir :clockwise 1 -1) (/ twist steps))]))))))

(defn helix-extrude
  " shape_pts : A list of points represent a shape. See the example below.
       radius : The radius of the cylinder. The radius of the cylinder. It also accepts a vector [r1, r2]. r1 is the bottom radius and r2 is the top radius of a cone.
       levels : The level count is performed every 360 degrees.
   level_dist : The distance between two vertial points.
       vt_dir : \"SPI_DOWN\" for spiraling down. \"SPI_UP\" for spiraling up. The default value is \"SPI_DOWN\".
       rt_dir : \"CT_CLK\" for counterclockwise. \"CLK\" for clockwise. The default value is \"CT_CLK\".
        twist : The number of degrees of through which the shape is extruded.
        scale : Scales the 2D shape by this value over the length of the extrusion. Scale can be a scalar or a vector.
    triangles : \"SOLID\" (default), \"HOLLOW\" or user-defined indexes. See polysections for details.
$fa, $fs, $fn : Check the cylinder module for more details."
  [& {:keys [shape-pts
             radius
             levels
             level-dist
             vt-dir
             rt-dir
             twist
             scale
             triangles]
      :or {vt-dir "SPI_DOWN"
           rt-dir "CT_CLK"
           twist 180
           scale 1
           triangles "SOLID"}}]
  `(:helix-extrude ~shape-pts ~radius ~levels ~level-dist ~vt-dir ~rt-dir ~twist ~scale ~triangles))

(defn circle-shell
  ([or ir]
   (m/difference (m/circle or) (m/circle ir)))
  ([or ir & {:keys [height center offset] :or {height 1 center false offset 0}}]
   (m/difference
    (m/cylinder or height :center center)
    (->> (m/cylinder ir height :center center)
         (m/translate [0 0 offset])))))

(defn square
  [x y & {:keys [fillet center] :or {center true fillet 0}}]
  (if (pos? fillet)
    (->> (m/square (- x (* 2 fillet)) (- y (* 2 fillet)) :center center)
         (m/minkowski (m/circle fillet)))
    (m/square x y :center center)))

(defn square-shell
  ([x y t & {:keys [center height offset] :or {center true offset 0}}]
   (cond->> (m/difference (m/square x y :center center)
                          (m/square (- x (* 2 t)) (- y (* 2 t)) :center center))
     height (m/extrude-linear {:height height :center false}) )))

(defn torus
  ([or ir & {:keys [angle shell] :or {angle 360}}]
   (let [thickness (- or ir)]
     (cond-> (->> (m/circle thickness)
                  (m/translate [(+ ir (half thickness)) 0 0])
                  (m/extrude-rotate {:angle angle}))
       shell (m/difference (torus (- or shell) (+ ir shell) :angle angle))))))

(defn half-sphere [r]
  (m/difference (m/sphere r)
                (->> (m/cube (* 2 r) (* 2 r) (* 2 r))
                     (m/translate [0 0 (- r)]))))

(defn hull
  [bottom-shape top-shape height]
  (m/hull (m/extrude-linear {:height 0.1 :center false} bottom-shape)
          (->> top-shape
               (m/extrude-linear {:height 0.1 :center false})
               (m/translate [0 0 (- height 0.1)]))))

(defn hull-shell
  [bottom-shape top-shape height thickness]
  (let [min (m/circle thickness)
        inner-hull (hull bottom-shape top-shape height)
        outer-hull (hull (m/minkowski min bottom-shape)
                         (m/minkowski min top-shape)
                         height)]
    (m/difference outer-hull inner-hull)))

(defmethod s/write-expr :bolt
  [depth [_ type turns higbee_arc]]
  (list (apply str (repeat depth " "))
        "bolt(" type ",turns=" turns ",higbee_arc=" higbee_arc ");"))

(defmethod s/write-expr :screw-by-pitch
  [depth [_ pitch d0 dr length flat]]
  (list (apply str (repeat depth " "))
        "screwByPitch(" pitch ",d0=" d0 ",dr=" dr ",length=" length, ",flat=" flat ");\n"))

(defmethod s/write-expr :screw-by-twist
  [depth [_ twist d0 dr length flat]]
  (list (apply str (repeat depth " "))
        "screwByTwist(twist=" twist ",d0=" d0 ",dr=" dr ",length=" length, ",flat=" flat ");\n"))

(defn to-array-string [v]
  (format "[%s]"
          (string/join ", "
                       (map (fn [point]
                              (format "[%s]" (string/join ", " point)))
                            v))))

(defmethod s/write-expr :helix-extrude
  [depth [_ shape-pts radius levels level-dist vt-dir rt-dir twist scale triangles]]
  (let [dyn-vars (str
                  (when m/*fn* (str ", $fn=" m/*fn*))
                  (when m/*fa* (str ", $fa=" m/*fa*))
                  (when m/*fs* (str ", $fs=" m/*fs*)))]
    (list (apply str (repeat depth " "))
          "helix_extrude(shape_pts=" (to-array-string shape-pts) ", radius=" radius
          ", levels=" levels ", level_dist=" level-dist
          (format ", vt_dir=\"%s\"" vt-dir)
          (format ", rt_dir=\"%s\"" rt-dir) ", twist=" twist
          ", scale=" scale
          (format ", triangles=\"%s\"" triangles)
          dyn-vars
          ");")))

(defn bolt
  ([type turns higbee-arc]
   `(:bolt ~type ~turns ~higbee-arc))
  ([type turns]
   (bolt type turns 30))
  ([type]
   (bolt type 5)))

(defn screw-by-pitch
  ([& {:keys [pitch d0 dr length flat]
       :or {pitch 3.6
            d0 12
            dr 1.5
            length 12
            flat 0.6}}]
   `(:screw-by-pitch ~pitch ~d0 ~dr ~length ~flat)))

(defn screw-by-twist
  ([& {:keys [twist d0 dr length flat]
       :or {twist (* 4 360)
            d0 12
            dr 1.5
            length 12
            flat 0.6}}]
   `(:screw-by-twist ~twist ~d0 ~dr ~length ~flat)))
