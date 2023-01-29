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

;;; Camera System Functions (Module: rcamera)

(defun update-camera (camera)
  (claylib/ll:update-camera (c-struct camera)))



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
  (/= 0 (claylib/ll:check-collision-circles (c-struct (pos circle1))
                                            (float (radius circle1))
                                            (c-struct (pos circle2))
                                            (float (radius circle2)))))

(defun check-collision-circle-rec (circle rec)
  "Check collision between CIRCLE and RECTANGLE."
  (check-type circle circle)
  (check-type rec rl-rectangle)
  (/= 0 (claylib/ll:check-collision-circle-rec (c-struct (pos circle))
                                               (float (radius circle))
                                               (c-struct rec))))

(defun-pt-bool check-collision-point-rec claylib/ll:check-collision-point-rec
  "Check if POINT is inside RECTANGLE."
  (point rl-vector2)
  (rec rl-rectangle))

(defun check-collision-point-circle (point circle)
  "Check if POINT is inside CIRCLE."
  (check-type point rl-vector2)
  (check-type circle circle)
  (/= 0 (claylib/ll:check-collision-point-circle (c-struct point)
                                                 (c-struct (pos circle))
                                                 (float (radius circle)))))

(defun check-collision-point-triangle (point triangle)
  "Check if POINT is inside TRIANGLE."
  (check-type point rl-vector2)
  (check-type triangle triangle)
  (/= 0 (claylib/ll:check-collision-point-triangle (c-struct point)
                                                   (c-struct (v1 triangle))
                                                   (c-struct (v2 triangle))
                                                   (c-struct (v3 triangle)))))

(defun check-collision-lines (line1 line2 &optional (retval (make-vector2 0 0)))
  "Check the collision between two straight lines. Allocates a new VECTOR2 unless you pass one.
Returns the vector if a collision exists, otherwise NIL."
  (check-type line1 line)
  (check-type line2 line)
  (check-type retval rl-vector2)
  (if (= 0 (claylib/ll:check-collision-lines (c-struct (start line1))
                                             (c-struct (end line1))
                                             (c-struct (start line2))
                                             (c-struct (end line2))
                                             (c-struct retval)))
      nil
      retval))

(defun check-collision-point-line (point line threshold)
  "Check if POINT belongs to a (straight) LINE with defined margin in pixels [threshold]."
  (check-type point rl-vector2)
  (check-type line line)
  (check-type threshold integer)
  (/= 0 (claylib/ll:check-collision-point-line (c-struct point)
                                               (c-struct (start line))
                                               (c-struct (end line))
                                               threshold)))

(defun get-collision-rec (rec1 rec2 &key (result-rec nil))
  "Get the collision rectangle for the collision of two rectangles REC1 and REC2.

This returns a newly allocated RECTANGLE unless RESULT-REC is given, in which case set RESULT-REC's
postion and dimensions to the reflect the result."
  (check-type rec1 rl-rectangle)
  (check-type rec2 rl-rectangle)
  (let ((retval (or result-rec (make-simple-rec 0 0 0 0))))
    (claylib/ll:get-collision-rec (c-struct retval) (c-struct rec1) (c-struct rec2))
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
  (claylib/ll:export-image (c-struct image)
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
  (set-slot :texture rt (texture rt))
  (unless (slot-boundp rt '%depth)
    (setf (slot-value rt '%depth) (make-instance 'texture)
          (c-struct (texture rt)) (claylib/ll:render-texture.texture (c-struct rt))))
  (set-slot :depth rt (make-instance 'texture))
  rt)

;; Color/pixel related functions

(defun-pt-arg0 fade claylib/ll:fade (make-color 0 0 0)
  "Destructively fade a color in/out to a specified alpha value, unless ALLOCATE-P is T,
in which case create a new COLOR object as the return value."
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
  (claylib/ll:measure-text-ex (c-struct vector)
                              (c-struct (font text))
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
  (claylib/ll:load-model-from-mesh (c-struct model) (c-struct mesh))
  (let ((c-meshes (autowrap:c-aref (model.meshes (c-struct model)) 0 'claylib/ll:mesh))
        (c-materials (autowrap:c-aref (model.materials (c-struct model)) 0 'claylib/ll:material)))
    (set-slot :transform model (transform model))
    (setf (meshes model)
          (make-instance 'rl-meshes :cl-array (make-rl-*-array c-meshes (mesh-count model)))

          (materials model)
          (make-instance 'rl-materials
                         :cl-array (make-rl-*-array c-materials (material-count model)))

          (pos model)
          (make-vector3 0 0 0))
    model))


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
  (claylib/ll:update-model-animation (c-struct model)
                                     (c-struct (elt (animations model) anim-index))
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
  (= 1 (claylib/ll:check-collision-box-sphere (c-struct box)
                                              (c-struct (pos sphere))
                                              (radius sphere))))

(defun-pt get-ray-collision-box claylib/ll:get-ray-collision-box
  "Gets a collision box for the passed ray and bounding box.
Allocates a new RAY-COLLISION unless you pass one."
  (rc rl-ray-collision nil (make-ray-collision 0 0 0 0 0 0))
  (ray ray)
  (box rl-bounding-box))



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
