(in-package #:claylib)

;;;; Raylib 'pass-through' functions
;;;; These functions mirror their Raylib equivalents with some massaging of arguments.
;;;; See the DEFUN-PT and DEFUN-PT-ARG0 macro definitions.

;;; Core

;; Screen-space-related functions

(defun-pt-arg0 get-world-to-screen-2d claylib/ll:get-world-to-screen-2d (make-vector2 0 0)
  "Get world-to-screen transform, a RL-VECTOR2. Destructively modifies the first arg
unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector2)
  (camera rl-camera-2d))

(defun-pt-arg0 get-screen-to-world-2d claylib/ll:get-screen-to-world-2d (make-vector2 0 0)
  "Get screen-to-world transform, a RL-VECTOR2. Destructively modifies the first arg
unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector2)
  (camera rl-camera-2d))

(defun-pt get-world-to-screen-3d claylib/ll:get-world-to-screen-ex
  "Get world-to-screen transform of a 3D camera. Allocates a new VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0))
  (position rl-vector3)
  (camera camera-3d)
  (width integer nil *screen-width*)
  (height integer nil *screen-height*))

(defun-pt get-mouse-ray claylib/ll:get-mouse-ray
  "Gets a mouse ray for the passed mouse position and camera. Allocates a new RAY unless you pass one."
  (ray ray nil (make-ray 0 0 0 0 0 0 +black+))
  (mouse-pos rl-vector2)
  (camera camera-3d))

;; Input-related functions: mouse

(defun-pt get-mouse-position claylib/ll:get-mouse-position
  "Get the current mouse position as a RL-VECTOR2. Allocates a new RL-VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0)))

(defun-pt get-mouse-delta claylib/ll:get-mouse-delta
  "Get difference in mouse position between frames. Allocates a new RL-VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0)))

(defun-pt get-mouse-wheel-move-v claylib/ll:get-mouse-wheel-move-v
  "Get mouse wheel movement as a RL-VECTOR2. Allocates a new RL-VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0)))

;;; Camera System Functions (Module: rcamera)

(defun update-camera (camera)
  (if (= (mode camera) +camera-pro+)
      (claylib/ll:update-camera-pro (c-ptr camera)
                                    (c-ptr (movement camera))
                                    (c-ptr (rot camera))
                                    (zoom camera))
      (claylib/ll:update-camera (c-ptr camera) (mode camera))))



;;; Basic Shapes Drawing Functions (Module: shapes)

;; Basic shapes collision detection functions

(defun-pt-bool check-collision-recs claylib/ll:check-collision-recs
  "Check collision between two rectangles."
  (rec1 rl-rectangle)
  (rec2 rl-rectangle))

(defun check-collision-circles (circle1 circle2)
  "Check collision between two circles."
  (check-type circle1 circle)
  (check-type circle2 circle)
  (claylib/ll:check-collision-circles (c-ptr (pos circle1))
                                      (float (radius circle1))
                                      (c-ptr (pos circle2))
                                      (float (radius circle2))))

(defun check-collision-circle-rec (circle rec)
  "Check collision between CIRCLE and RECTANGLE."
  (check-type circle circle)
  (check-type rec rl-rectangle)
  (claylib/ll:check-collision-circle-rec (c-ptr (pos circle))
                                         (float (radius circle))
                                         (c-ptr rec)))

(defun-pt-bool check-collision-point-rec claylib/ll:check-collision-point-rec
  "Check if POINT is inside RECTANGLE."
  (point rl-vector2)
  (rec rl-rectangle))

(defun check-collision-point-circle (point circle)
  "Check if POINT is inside CIRCLE."
  (check-type point rl-vector2)
  (check-type circle circle)
  (claylib/ll:check-collision-point-circle (c-ptr point)
                                           (c-ptr (pos circle))
                                           (float (radius circle))))

(defun check-collision-point-triangle (point triangle)
  "Check if POINT is inside TRIANGLE."
  (check-type point rl-vector2)
  (check-type triangle triangle)
  (claylib/ll:check-collision-point-triangle (c-ptr point)
                                             (c-ptr (v1 triangle))
                                             (c-ptr (v2 triangle))
                                             (c-ptr (v3 triangle))))

(defun check-collision-lines (line1 line2 &optional (retval (make-vector2 0 0)))
  "Check the collision between two straight lines. Allocates a new VECTOR2 unless you pass one.
Returns the vector if a collision exists, otherwise NIL."
  (check-type line1 line)
  (check-type line2 line)
  (check-type retval rl-vector2)
  (when (claylib/ll:check-collision-lines (c-ptr (start line1))
                                          (c-ptr (end line1))
                                          (c-ptr (start line2))
                                          (c-ptr (end line2))
                                          (c-ptr retval))
    retval))

(defun check-collision-point-line (point line threshold)
  "Check if POINT belongs to a (straight) LINE with defined margin in pixels [threshold]."
  (check-type point rl-vector2)
  (check-type line line)
  (check-type threshold integer)
  (claylib/ll:check-collision-point-line (c-ptr point)
                                         (c-ptr (start line))
                                         (c-ptr (end line))
                                         threshold))

(defun get-collision-rec (rec1 rec2 &key (result-rec nil))
  "Get the collision rectangle for the collision of two rectangles REC1 and REC2.

This returns a newly allocated RECTANGLE unless RESULT-REC is given, in which case set RESULT-REC's
postion and dimensions to the reflect the result."
  (check-type rec1 rl-rectangle)
  (check-type rec2 rl-rectangle)
  (let ((retval (or result-rec (make-simple-rec 0 0 0 0))))
    (claylib/ll:get-collision-rec (c-ptr retval) (c-ptr rec1) (c-ptr rec2))
    retval))



;;; Textures

;; Image loading functions

(defun-pt load-image-from-texture claylib/ll:load-image-from-texture
  "Load an image from the given TEXTURE. Allocates a new RL-IMAGE unless you pass one."
  (image rl-image nil (make-instance 'rl-image))
  (texture rl-texture nil))

(defun export-image (image filepath &key)
  "Export image data to FILEPATH."
  (check-type image rl-image)
  (check-type filepath (or pathname string))
  (claylib/ll:export-image (c-ptr image)
                           (namestring filepath))
  image)

;; Image generation functions

(defun-pt gen-image-checked claylib/ll:gen-image-checked
  "Generate a checkerboard image. Allocates a new RL-IMAGE unless you pass one."
  (image rl-image nil (make-instance 'rl-image))
  (width integer)
  (height integer)
  (checks-x integer)
  (checks-y integer)
  (color1 rl-color)
  (color2 rl-color))

;; Image manipulation functions

(defun image-crop (image crop)
  "Crop an image to a defined rectangle."
  (check-type image (or rl-image image))
  (check-type crop rl-rectangle)
  (claylib/ll:image-crop (c-ptr image) (c-ptr crop)))

(defun image-resize (image new-width new-height &key (nn nil))
  "Resize image using the Bicubic scaling algorithm, or Nearest-Neighbor when NN is T."
  (check-type image (or rl-image image))
  (check-type new-width integer)
  (check-type new-height integer)
  (funcall (if nn
               #'claylib/ll:image-resize-nn
               #'claylib/ll:image-resize)
           (c-ptr image) new-width new-height))

(defun image-flip-vertical (image)
  "Flip IMAGE vertically."
  (check-type image (or rl-image image))
  (claylib/ll:image-flip-vertical (c-ptr image))
  image)

(defun image-flip-horizontal (image)
  "Flip IMAGE horizontally."
  (check-type image (or rl-image image))
  (claylib/ll:image-flip-horizontal (c-ptr image))
  image)

;; Texture loading functions

(defun-pt load-texture-from-image claylib/ll:load-texture-from-image
  "Load a texture from a passed-in image. Allocates a new RL-TEXTURE unless you pass one."
  (texture rl-texture nil (make-instance 'rl-texture))
  (image rl-image))

(defun load-render-texture (width height &key (rt (make-instance 'rl-render-texture)))
  (check-type width integer)
  (check-type height integer)
  (check-type rt rl-render-texture)
  (claylib/ll:load-render-texture (c-ptr rt) width height)
  (unless (slot-boundp rt '%texture)
    (setf (slot-value rt '%texture)
          (make-instance 'texture
                         :c-ptr (field-value (c-ptr rt) 'render-texture 'texture)
                         :finalize nil)))
  (set-slot :texture rt (texture rt))
  (unless (slot-boundp rt '%depth)
    (setf (slot-value rt '%depth)
          (make-instance 'texture
                         :c-ptr (field-value (c-ptr rt) 'render-texture 'texture)
                         :finalize nil)))
  (set-slot :depth rt (make-instance 'texture))
  rt)

;; Color/pixel related functions

(defun-pt-arg0 fade claylib/ll:fade (make-color 0 0 0)
  "Destructively fade a color in/out to a specified alpha value, unless ALLOCATE-OR-INTO is passed."
  (color rl-color)
  (alpha number float))

(defun-pt get-color claylib/ll:get-color
  "Get a color object from a hex value. Allocates a new COLOR unless you pass one."
  (color rl-color nil (make-instance 'color))
  (hex-value number))



;;; Font Loading and Text Drawing Functions (Module: text)

;; Text font info functions

(defun measure-text-ex (text &key (vector (make-vector2 0 0)))
  "Returns an RL-VECTOR2 with the width (x) and height (y) of the TEXT object accounting for its
font, size and spacing. Allocates a new RL-VECTOR2 unless you pass one."
  (check-type text text)
  (claylib/ll:measure-text-ex (c-ptr vector)
                              (c-ptr (font text))
                              (text text)
                              (size text)
                              (spacing text))
  vector)

(defun-pt-num get-glyph-index claylib/ll:get-glyph-index
  "Get glyph index position in font for a codepoint (Unicode character). '?' if not found."
  (font rl-font)
  (codepoint integer))



;;; Basic 3d Shapes Drawing Functions (Module: models)

;; Basic geometric 3D shapes drawing functions

(defun-pt-void draw-cube claylib/ll:draw-cube
  "Draw a cube from given properties (not a cube object)."
  (position rl-vector3)
  (width number float)
  (height number float)
  (length number float)
  (color rl-color))



;;; Models 3d Loading and Drawing Functions (Module: models)

;; Model loading/unloading functions

;; TODO generalize for all functions that do model creation
(defun load-model-from-mesh (mesh &key (model (make-instance 'model)))
  "Load a model from a passed-in mesh. Allocates a new RL-MODEL unless you pass one."
  (check-type mesh rl-mesh)
  (check-type model rl-model)
  (claylib/ll:load-model-from-mesh (c-ptr model) (c-ptr mesh))
  (tg:cancel-finalization (slot-value mesh '%c-ptr))  ; Avoid double free -- mesh is now part of model.
  (let ((c-meshes (cffi:mem-ref (field-ptr (c-ptr model) 'model 'meshes) :pointer))
        (c-materials (cffi:mem-ref (field-ptr (c-ptr model) 'model 'materials) :pointer)))
    (set-slot :transform model (transform model))
    (setf (meshes model)
          (make-instance 'rl-meshes :cl-array (make-rl-mesh-array c-meshes (mesh-count model)))

          (materials model)
          (make-instance 'rl-materials
                         :cl-array (make-rl-material-array c-materials (material-count model)))

          (pos model)
          (make-vector3 0 0 0))
    model))

(defun-pt get-model-bounding-box claylib/ll:get-model-bounding-box
  "Compute a bounding box for a model. Allocates a new RL-BOUNDING-BOX unless you pass one."
  (bounding-box rl-bounding-box nil (make-instance 'rl-bounding-box))
  (model rl-model))

;; Mesh generation functions

(defun-pt gen-mesh-poly claylib/ll:gen-mesh-poly
  "Generate a polygonal mesh. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (sides integer)
  (radius number float))

(defun-pt gen-mesh-plane claylib/ll:gen-mesh-plane
  "Generate plane mesh (with subdivisions). Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (width number float)
  (length number float)
  (res-x integer)
  (res-z integer))

(defun-pt gen-mesh-cube claylib/ll:gen-mesh-cube
  "Generate a mesh rectangular prism, a.k.a. 'cube.' Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (width number float)
  (height number float)
  (length number float))

(defun-pt gen-mesh-sphere claylib/ll:gen-mesh-sphere
  "Generate a standard sphere mesh. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (rings integer)
  (slices integer))

(defun-pt gen-mesh-hemisphere claylib/ll:gen-mesh-hemi-sphere
  "Generate a half-sphere mesh (no bottom cap). Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (rings integer)
  (slices integer))

(defun-pt gen-mesh-cylinder claylib/ll:gen-mesh-cylinder
  "Generate a mesh cylinder. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (height number float)
  (slices integer))

(defun-pt gen-mesh-cone claylib/ll:gen-mesh-cone
  "Generate a cone/pyramid mesh. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (height number float)
  (slices integer))

(defun-pt gen-mesh-torus claylib/ll:gen-mesh-torus
  "Generate a torus mesh. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (size number float)
  (rad-seg integer)
  (sides integer))

(defun-pt gen-mesh-knot claylib/ll:gen-mesh-knot
  "Generate a trefoil knot mesh. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (size number float)
  (rad-seg integer)
  (sides integer))

(defun-pt gen-mesh-heightmap claylib/ll:gen-mesh-heightmap
  "Generate heightmap mesh from image data. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (heightmap rl-image)
  (size rl-vector3))

(defun-pt gen-mesh-cubicmap claylib/ll:gen-mesh-cubicmap
  "Generate cubes-based map mesh from image data. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (cubicmap rl-image)
  (cube-size rl-vector3))

;; Material loading/unloading functions

(defun-pt-void set-material-texture claylib/ll:set-material-texture
  "Set texture for a material map type (+MATERIAL-MAP-DIFFUSE+, +MATERIAL-MAP-SPECULAR+...)"
  (material rl-material)
  (map-type integer)
  (texture rl-texture))

;; Model animations loading/unloading functions

(defun update-model-animation (model anim-index frame)
  "Updates MODEL's animation pose based on the ANIM-INDEX and FRAME."
  (check-type model model)
  (check-type frame (integer 0 *))
  (check-type anim-index (integer 0 *))
  (unless (animations model)
    (error "~a has no associated animations." model))
  (claylib/ll:update-model-animation (c-ptr model)
                                     (c-ptr (elt (animations model) anim-index))
                                     frame))

;; Collision detection functions

(defun-pt-bool check-collision-boxes claylib/ll:check-collision-boxes
  "Check collision between two bounding boxes."
  (box1 rl-bounding-box)
  (box2 rl-bounding-box))

(defun check-collision-box-sphere (box sphere)
  "Check collision between bounding box and a sphere."
  (check-type box rl-bounding-box)
  (check-type sphere sphere)
  (claylib/ll:check-collision-box-sphere (c-ptr box)
                                         (c-ptr (pos sphere))
                                         (radius sphere)))

(defun-pt get-ray-collision-box claylib/ll:get-ray-collision-box
  "Gets a collision box for the passed ray and bounding box.
Allocates a new RAY-COLLISION unless you pass one."
  (rc rl-ray-collision nil (make-ray-collision 0 0 0 0 0 0))
  (ray ray)
  (box rl-bounding-box))



;;; Raymath

;; Vector2

(defun-pt-arg0 vector2-subtract claylib/ll:vector2-subtract (make-vector2 0 0)
  "Subtract two RL-VECTOR2s. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (v1 rl-vector2)
  (v2 rl-vector2))

(defun-pt-arg0 vector2-add claylib/ll:vector2-add (make-vector2 0 0)
  "Add two RL-VECTOR2s. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (v1 rl-vector2)
  (v2 rl-vector2))

(defun-pt-arg0 vector2-scale claylib/ll:vector2-scale (make-vector2 0 0)
  "Scale a RL-VECTOR2. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector2)
  (scale number float))

(defun vector2-length (vec)
  (claylib/ll:vector2-length (c-ptr vec)))

;; Vector3

(defun-pt-arg0 vector3-add claylib/ll:vector3-add (make-vector3 0 0 0)
  "Add two RL-VECTOR3s. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (v1 rl-vector3)
  (v2 rl-vector3))

(defun-pt-arg0 vector3-subtract claylib/ll:vector3-subtract (make-vector3 0 0 0)
  "Subtract two RL-VECTOR3s. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (v1 rl-vector3)
  (v2 rl-vector3))

(defun-pt-num vector3-angle claylib/ll:vector3-angle
  "Calculate the angle between two RL-VECTOR3s."
  (v1 rl-vector3)
  (v2 rl-vector3))

(defun-pt-arg0 vector3-negate claylib/ll:vector3-negate (make-vector3 0 0 0)
  "Negate a RL-VECTOR3. Destructively modifies the passed vector unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector3))

(defun-pt-arg0 vector3-normalize claylib/ll:vector3-normalize (make-vector3 0 0 0)
  "Normalize a RL-VECTOR3. Destructively modifies the passed vector unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector3))

(defun-pt-arg0 vector3-transform claylib/ll:vector3-transform (make-vector3 0 0 0)
  "Transform a RL-VECTOR3 by a RL-MATRIX. Destructively modifies the passed vector unless
ALLOCATE-OR-INTO is passed."
  (vec rl-vector3)
  (mat rl-matrix))

(defun-pt-arg0 vector3-rotate-by-axis-angle claylib/ll:vector3-rotate-by-axis-angle (make-vector3 0 0 0)
  "Rotate a RL-VECTOR3 around a given axis (RL-VECTOR3) by ANGLE. Destructively modifies the first
arg unless ALLOCATE-OR-INTO is passed."
  (vec rl-vector3)
  (axis rl-vector3)
  (angle number float))

(defun-pt-num vector3-distance claylib/ll:vector3-distance
  "Compute the distance between two RL-VECTOR3s."
  (v1 rl-vector3)
  (v2 rl-vector3))

;; Quaternion

(defun-pt-arg0 quaternion-multiply claylib/ll:quaternion-multiply (make-vector4 0 0 0 0)
  "Multiply two RL-VECTOR4s. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (q1 rl-vector4)
  (q2 rl-vector4))

(defun-pt quaternion-to-matrix claylib/ll:quaternion-to-matrix
  "Convert a quaternion (RL-VECTOR4) to a matrix. Allocates a new RL-MATRIX unless you pass one."
  (matrix rl-matrix nil (make-zero-matrix))
  (quat rl-vector4))

(defun-pt quaternion-from-matrix claylib/ll:quaternion-from-matrix
  "Convert a RL-MATRIX to a quaternion (RL-VECTOR4). Allocates a new RL-VECTOR4 unless you pass one."
  (quat rl-vector4 nil (make-vector4 0 0 0 0))
  (matrix rl-matrix))

(defun-pt quaternion-from-euler claylib/ll:quaternion-from-euler
  "Calculate a quaternion from PITCH, YAW, and ROLL. Allocates a new RL-VECTOR4 unless you pass one."
  (quat rl-vector4 nil (make-vector4 0 0 0 0))
  (pitch number float)
  (yaw number float)
  (roll number float))

(defun-pt quaternion-to-euler claylib/ll:quaternion-to-euler
  "Convert a quaternion (RL-VECTOR4) to a RL-VECTOR3. Allocates a new RL-VECTOR3 unless you pass one."
  (vec rl-vector3 nil (make-vector3 0 0 0))
  (quat rl-vector4))

;; Matrix

(defun-pt-arg0 matrix-multiply claylib/ll:matrix-multiply (make-zero-matrix)
  "Multiply two matrices. Destructively modifies the first arg unless ALLOCATE-OR-INTO is passed."
  (m1 rl-matrix)
  (m2 rl-matrix))

(defun-pt matrix-rotate-zyx claylib/ll:matrix-rotate-zyx
  "Get ZYX rotation matrix from a RL-VECTOR3. Allocates a new RL-MATRIX unless you pass one."
  (matrix rl-matrix nil (make-zero-matrix))
  (ang rl-vector3))



;;; rcamera

(defun-pt get-camera-forward claylib/ll:get-camera-forward
  "Get camera's forward vector (normalized). Allocates a new RL-VECTOR3 unless you pass one."
  (vec rl-vector3 nil (make-vector3 0 0 0))
  (camera rl-camera-3d))

(defun-pt get-camera-up claylib/ll:get-camera-up
  "Get camera's up vector (normalized). Allocates a new RL-VECTOR3 unless you pass one."
  (vec rl-vector3 nil (make-vector3 0 0 0))
  (camera rl-camera-3d))

(defun-pt get-camera-right claylib/ll:get-camera-right
  "Get camera's right vector (normalized). Allocates a new RL-VECTOR3 unless you pass one."
  (vec rl-vector3 nil (make-vector3 0 0 0))
  (camera rl-camera-3d))

(defun-pt-void camera-move-forward claylib/ll:camera-move-forward
  "Move camera along its forward axis."
  (camera rl-camera-3d)
  (distance number single-float)
  (move-in-world-plane boolean))

(defun-pt-void camera-move-up claylib/ll:camera-move-up
  "Move camera along its up axis."
  (camera rl-camera-3d)
  (distance number single-float))

(defun-pt-void camera-move-right claylib/ll:camera-move-right
  "Move camera along its right axis."
  (camera rl-camera-3d)
  (distance number single-float)
  (move-in-world-plane boolean))

(defun-pt-void camera-move-to-target claylib/ll:camera-move-to-target
  "Move camera closer to or farther from its target."
  (camera rl-camera-3d)
  (delta number single-float))

(defun-pt-void camera-yaw claylib/ll:camera-yaw
  "Rotate the camera around its up vector (looking left/right), or around the target.
Angle is in radians."
  (camera rl-camera-3d)
  (angle number single-float)
  (rotate-around-target boolean))

(defun-pt-void camera-pitch claylib/ll:camera-pitch
  "Rotate the camera around its right vector (looking up/down), or around the target.
lock-view prevents overrotation ('somersaults'), rotate-up adjusts up vector. Angle in radians."
  (camera rl-camera-3d)
  (angle number single-float)
  (lock-view boolean nil t)
  (rotate-around-target boolean)
  (rotate-up boolean))

(defun-pt-void camera-roll claylib/ll:camera-roll
  "Rotate the camera around its forward vector (do a barrel roll!). Angle is in radians."
  (camera rl-camera-3d)
  (angle number single-float))

(defun-pt get-camera-view-matrix claylib/ll:get-camera-view-matrix
  "Get the camera view matrix. Allocates a new RL-MATRIX unless you pass one."
  (matrix rl-matrix nil (make-zero-matrix))
  (camera rl-camera-3d))

(defun-pt get-camera-projection-matrix claylib/ll:get-camera-projection-matrix
  "Get the camera projection matrix. Allocates a new RL-MATRIX unless you pass one."
  (matrix rl-matrix nil (make-zero-matrix))
  (camera rl-camera-3d)
  (aspect number single-float))



;; Music management

(defun-pt-bool is-music-stream-playing-p claylib/ll:is-music-stream-playing-p
  "Check is MUSIC is playing."
  (music rl-music))

(defun-pt-void seek-music-stream claylib/ll:seek-music-stream
  "Seek MUSIC to a position (in seconds)."
  (music rl-music)
  (position number float))

(defun-pt-num get-music-time-length claylib/ll:get-music-time-length
  "Get MUSIC time length (in seconds)."
  (music rl-music))

(defun-pt-num get-music-time-played claylib/ll:get-music-time-played
  "Get MUSIC time played (in seconds)."
  (music rl-music))



;;; Raygui

(defun gui-load-style (path)
  "Load a new GUI style from PATH. PATH must be the name of a .rgs binary file."
  (check-type path (or pathname string))
  (claylib/ll:gui-load-style (namestring path)))
