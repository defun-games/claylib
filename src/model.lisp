(in-package #:claylib)

(defclass rl-model ()
  ((%transform :initarg :transform
               :type rl-matrix
               :accessor transform)
   (%meshes :initarg :meshes
                                        ; TODO: type (pointer)
            :accessor meshes)
   (%materials :initarg :materials
                                        ; TODO: type (pointer)
               :accessor materials)
   (%bones :initarg :bones
                                        ; TODO: type (pointer)
           :accessor bones)
   (%bind-pose :initarg :bind-pose
               :type rl-transform ; pointer
               :accessor bind-pose)
   (%c-struct
    :type claylib/ll:model
    :initform (autowrap:alloc 'claylib/ll:model)
    :accessor c-struct)))

(defcreader mesh-count rl-model mesh-count model)
(defcreader material-count rl-model material-count model)
(defcreader mesh-material rl-model mesh-material model) ; pointer
(defcreader bone-count rl-model bone-count model)

(defcwriter mesh-count rl-model mesh-count model integer)
(defcwriter material-count rl-model material-count model integer)
(defcwriter mesh-material rl-model mesh-material model integer) ; pointer
(defcwriter bone-count rl-model bone-count model integer)
(defcwriter-struct transform rl-model transform model matrix
  m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)
(defcwriter-struct meshes rl-model meshes model mesh ; pointer
  vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors
  indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)
(defcwriter-struct materials rl-model materials model material ; pointer
  shader maps params)
(defcwriter-struct bones rl-model bones model bone-info ; pointer
  name parent)
(defcwriter-struct bind-pose rl-model bind-pose model transform ; pointer
  trans rot scale)

(definitializer rl-model
  (transform rl-matrix) (mesh-count integer) (material-count integer) (meshes rl-mesh)
  (materials rl-material) (mesh-material integer) (bone-count integer)
  (bones rl-bone-info) (bind-pose rl-transform))

(default-free rl-model)
(default-free-c claylib/ll:model unload-model t)



(defclass model (rl-model 3d-object)
  ((%scale :initarg :scale
           :type rl-vector3
           :accessor scale)
   (%tint :initarg :tint
          :type rl-color
          :accessor tint)
   (%filled :initarg :filled
            :type boolean
            :accessor filled)
   (%asset :initarg :asset
           :type model-asset
           :accessor asset)))

(definitializer model (scale rl-vector3 nil) (tint rl-color nil))

(default-slot-value model %scale (make-vector3 1 1 1))
(default-slot-value model %tint +white+)
(default-slot-value model %filled t)

(defun make-model (model-asset x y z
                   &rest args &key scale tint rot-angle rot-axis filled
                                transform mesh-count material-count meshes
                                materials mesh-material bone-count bones
                                bind-pose)
  "Make a Claylib model.

Models are backed by RL-MODELs which draw reusable data from the given MODEL-ASSET."
  (declare (ignore scale tint rot-angle rot-axis filled))
  (load-asset model-asset)
  (let ((model (apply #'make-instance 'model
                      :allow-other-keys t
                      :asset model-asset
                      :pos (make-vector3 x y z)
                      args))
        (rl-asset model-asset))
    (setf (transform model) (or transform (transform rl-asset))
          (mesh-count model) (or mesh-count (mesh-count rl-asset))
          (material-count model) (or material-count (material-count rl-asset))
          (meshes model) (or meshes (meshes rl-asset))
          (materials model) (or materials (materials rl-asset))
          (mesh-material model) (or mesh-material (mesh-material rl-asset))
          (bone-count model) (or bone-count (bone-count rl-asset))
          (bones model) (or bones (bones rl-asset))
          (bind-pose model) (or bind-pose (bind-pose rl-asset))
          ; TODO: anims
          )
    model)
  ;; TODO: Set the rl-model fields to the model-asset data.
  ;; Either allow this in initargs above or use the cwriters here, e.g.
  ;;
  ;; (setf (meshes model) (meshes model-asset)          ; pointer, a proper re-use!
  ;;       (mesh-count model) (mesh-count model-asset)) ; integer, nothing to see here
  ;;
  ;; TODO: initialize fresh transforms and such
  )

(defmethod free ((obj model))
  (free (scale obj))
  (call-next-method))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-struct obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))

(defun extract-model-data (path)
  "Return a plist of the model data of interest in the file at PATH."
  (let* ((rl-model (make-instance 'rl-model))
         (c-model (c-struct rl-model)))
    (claylib/ll:load-model c-model (namestring path))
    ;; TODO: make copies of the following fields, need copy functions!
    (list :transform      (model.transform c-model)
          :mesh-count     (mesh-count rl-model)
          :material-count (material-count rl-model)
          :meshes         (model.meshes c-model)
          :materials      (model.materials c-model)
          :mesh-material  (mesh-material rl-model)
          :bone-count     (bone-count rl-model)
          :bones          (model.bones c-model)
          :bind-pose      (model.bind-pose c-model))
    ;; TODO: free rl-model
    ))
