(in-package #:claylib)

;;;; Raylib 'pass-through' functions
;;;; These functions mirror their Raylib equivalents with some massaging of arguments.
;;;; See the DEFUN-PT and DEFUN-PT-ARG0 macro definitions.

;;; Core

;; Screen-space-related functions

(defun-pt-arg0 get-world-to-screen-2d claylib/ll:get-world-to-screen2d (make-vector2 0 0)
  "Get world-to-screen transform, a RL-VECTOR2. Destructively modifies the first arg
unless ALLOCATE-P is T."
  (vec rl-vector2)
  (camera rl-camera-2d))

(defun-pt-arg0 get-screen-to-world-2d claylib/ll:get-screen-to-world2d (make-vector2 0 0)
  "Get screen-to-world transform, a RL-VECTOR2. Destructively modifies the first arg
unless ALLOCATE-P is T."
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

;; Camera System Functions (Module: rcamera)

(defun update-camera (camera)
  (claylib/ll:update-camera (c-struct camera)))



;;; Textures

;; Image loading functions

(defun-pt load-image-from-texture claylib/ll:load-image-from-texture
  "Load an image from the given TEXTURE. Allocates a new RL-IMAGE unless you pass one."
  (image rl-image nil (make-instance 'rl-image))
  (texture rl-texture nil))

(defun-pt export-image claylib/ll:export-image
  "Export image data to FILENAME."
  (image rl-image nil)
  (filename string))

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
  (claylib/ll:image-crop (c-struct image) (c-struct crop)))

(defun image-resize (image new-width new-height &key (nn nil))
  "Resize image using the Bicubic scaling algorithm, or Nearest-Neighbor when NN is T."
  (check-type image (or rl-image image))
  (check-type new-width integer)
  (check-type new-height integer)
  (funcall (if nn
               #'claylib/ll:image-resize-nn
               #'claylib/ll:image-resize)
           (c-struct image) new-width new-height))

(defun image-flip-vertical (image)
  "Flip IMAGE vertically."
  (check-type image (or rl-image image))
  (claylib/ll:image-flip-vertical (c-struct image))
  image)

(defun image-flip-horizontal (image)
  "Flip IMAGE horizontally."
  (check-type image (or rl-image image))
  (claylib/ll:image-flip-horizontal (c-struct image))
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
  (claylib/ll:load-render-texture (c-struct rt) width height)
  (unless (slot-boundp rt '%texture)
    (setf (slot-value rt '%texture) (make-instance 'texture)
          (c-struct (texture rt)) (claylib/ll:render-texture.texture (c-struct rt))))
  (set-slot :texture rt (texture rt) :free :never)
  (unless (slot-boundp rt '%depth)
    (setf (slot-value rt '%depth) (make-instance 'texture)
          (c-struct (texture rt)) (claylib/ll:render-texture.texture (c-struct rt))))
  (set-slot :depth rt (make-instance 'texture) :free :never)
  rt)

;; Color/pixel related functions

(defun-pt-arg0 fade claylib/ll:fade (make-color 0 0 0)
  "Destructively fade a color in/out to a specified alpha value, unless ALLOCATE-P is T,
in which case create a new COLOR object as the return value."
  (color rl-color)
  (alpha number float))



;;; Models

;; Model loading/unloading functions

(defun-pt load-model-from-mesh claylib/ll:load-model-from-mesh
  "Load a model from a passed-in mesh. Allocates a new RL-MODEL unless you pass one."
  (model rl-model nil (make-instance 'rl-model))
  (mesh rl-mesh))

;; Mesh generation functions

(defun-pt gen-mesh-cylinder claylib/ll:gen-mesh-cylinder
  "Generate a mesh cylinder. Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (radius number float)
  (height number float)
  (slices integer))

(defun-pt gen-mesh-cube claylib/ll:gen-mesh-cube
  "Generate a mesh rectangular prism, a.k.a. 'cube.' Allocates a new RL-MESH unless you pass one."
  (mesh rl-mesh nil (make-instance 'rl-mesh))
  (width number float)
  (height number float)
  (length number float))

;; Collision detection functions

(defun-pt get-ray-collision-box claylib/ll:get-ray-collision-box
  "Gets a collision box for the passed ray and bounding box.
Allocates a new RAY-COLLISION unless you pass one."
  (rc rl-ray-collision nil (make-ray-collision 0 0 0 0 0 0))
  (ray ray)
  (box rl-bounding-box))

(defun-pt-bool check-collision-point-rec claylib/ll:check-collision-point-rec
  "Check if POINT is inside RECTANGLE."
  (point rl-vector2)
  (rec rl-rectangle))



;;; Raymath

;; Vector2

(defun-pt-arg0 vector2-subtract claylib/ll:vector2-subtract (make-vector2 0 0)
  "Subtract two RL-VECTOR2s. Destructively modifies the first arg unless ALLOCATE-P is T."
  (v1 rl-vector2)
  (v2 rl-vector2))

(defun-pt-arg0 vector2-add claylib/ll:vector2-add (make-vector2 0 0)
  "Add two RL-VECTOR2s. Destructively modifies the first arg unless ALLOCATE-P is T."
  (v1 rl-vector2)
  (v2 rl-vector2))

(defun-pt-arg0 vector2-scale claylib/ll:vector2-scale (make-vector2 0 0)
  "Scale a RL-VECTOR2. Destructively modifies the first arg unless ALLOCATE-P is T."
  (vec rl-vector2)
  (scale number float))

(defun vector2-length (vec)
  (claylib/ll:vector2-length (c-struct vec)))

;; Quaternion

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

(defun-pt matrix-rotate-zyx claylib/ll:matrix-rotate-zyx
  "Get ZYX rotation matrix from a RL-VECTOR3. Allocates a new RL-MATRIX unless you pass one."
  (matrix rl-matrix nil (make-zero-matrix))
  (ang rl-vector3))
