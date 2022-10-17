(in-package #:claylib)
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-mesh-materials (sequences:sequence)
    ((%cl-array :type (array integer 1)
                :initarg :cl-array
                :reader cl-array
                :documentation "Simple array mapping meshes to their associated materials, per model."))))

(defmethod sequences:length ((sequence rl-mesh-materials))
  (length (cl-array sequence)))

(defmethod sequences:elt ((sequence rl-mesh-materials) index)
  (elt (cl-array sequence) index))

(defmethod (setf sequences:elt) ((value integer) (sequence rl-mesh-materials) index)
  (setf (aref (cl-array sequence) index) value)
  (c-let ((n :int :value value))  ; TODO: Technically a memory leak, but does it actually matter?
    (setf (autowrap:c-aref ))))
|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model ()
    ((%transform :initarg :transform
                 :type rl-matrix
                 :reader transform)
     (%meshes :initarg :meshes
              :type rl-meshes
              :reader meshes)
     (%materials :initarg :materials
                 :type rl-materials
                 :reader materials)
     (%bones :initarg :bones
             :type rl-bones
             :reader bones)
     (%bind-pose :initarg :bind-pose
                 :type rl-transforms
                 :reader bind-pose)
     (%animations :initarg :animations
                  :type rl-animations
                  :accessor animations)
     (%c-struct
      :type claylib/ll:model
      :initform (autowrap:calloc 'claylib/ll:model)
      :accessor c-struct))))

(defcreader mesh-count rl-model mesh-count model)
(defcreader material-count rl-model material-count model)
(defcreader bone-count rl-model bone-count model)
(defmethod mesh-material ((model rl-model) (index integer))
  (when (and (< index (mesh-count model))
             (>= index 0))
    (autowrap:c-aref (model.mesh-material (c-struct model)) index :int)))
(defmethod mesh-materials ((model rl-model))
  (loop for i below (mesh-count model)
        collect (mesh-material model i)))

(defcwriter mesh-count rl-model mesh-count model integer)
(defcwriter material-count rl-model material-count model integer)
(defcwriter bone-count rl-model bone-count model integer)
(defcwriter-struct transform rl-model transform model matrix
  m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)
(defcwriter-struct meshes rl-model meshes model mesh  ; TODO: Array/pointer
  vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors
  indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)
(defcwriter-struct materials rl-model materials model material  ; TODO: Array/pointer
  shader maps params)
(defcwriter-struct bones rl-model bones model bone-info ; TODO: Array/pointer
  name parent)
(defcwriter-struct bind-pose rl-model bind-pose model transform ; pointer
  trans rot scale)
(defmethod (setf mesh) ((value rl-mesh) (model rl-model) (index integer))
  (when (and (< index (mesh-count model))
             (>= index 0))
    (cffi:foreign-funcall "memcpy"
                          :pointer (autowrap:c-aptr (model.meshes (c-struct model))
                                                    index
                                                    'claylib/ll:mesh)
                          :pointer (autowrap:ptr (c-struct value))
                          :int +foreign-mesh-size+
                          :void)
#|    (setf (autowrap:c-aref (model.meshes model) index 'claylib/ll:mesh)
          (c-struct value))|#))
(defmethod (setf meshes) ((value rl-meshes) (model rl-model))
  (when (cffi-sys:null-pointer-p (model.meshes (c-struct model)))
    (setf (model.meshes (c-struct model))
          (autowrap:ptr (autowrap:calloc 'claylib/ll:mesh (length value)))))
  ;(set-slot :meshes model value)
  (dotimes (i (mesh-count model))
    (setf (mesh model i) (elt value i))))
(defmethod (setf material) ((value rl-material) (model rl-model) (index integer))
  (when (and (< index (material-count model))
             (>= index 0))
    (cffi:foreign-funcall "memcpy"
                          :pointer (autowrap:c-aptr (model.materials (c-struct model))
                                                    index
                                                    'claylib/ll:material)
                          :pointer (autowrap:ptr (c-struct value))
                          :int +foreign-material-size+
                          :void)))
(defmethod (setf materials) ((value rl-materials) (model rl-model))
  (when (cffi-sys:null-pointer-p (model.materials (c-struct model)))
    (setf (model.materials (c-struct model))
          (autowrap:ptr (autowrap:calloc 'claylib/ll:material (length value)))))
  (dotimes (i (material-count model))
    (setf (material model i) (elt value i))))
(defmethod (setf mesh-material) ((value integer) (model rl-model) (index integer))
  (when (and (< index (mesh-count model))
             (>= index 0)
             (< value (material-count model))
             (>= value 0))
    (claylib/ll:set-model-mesh-material (c-struct model) index value)))
(defmethod (setf mesh-materials) ((value sequence) (model rl-model))
  (when (cffi-sys:null-pointer-p (model.mesh-material (c-struct model)))
    (setf (model.mesh-material (c-struct model))
          (autowrap:ptr (autowrap:calloc :int (length value)))))
  (dotimes (i (mesh-count model))
    (setf (mesh-material model i) (if (< i (length value))
                                      (elt value i)
                                      0))))

(defmethod sync-children ((obj rl-model))
  (flet ((i0 (array type)
           (autowrap:c-aref array 0 type)))
    (let* ((c (c-struct obj))
           (tform (model.transform c))
           (meshes (model.meshes c))
           (mats (model.materials c))
           (bones (model.bones c))
           (bpose (model.bind-pose c)))
      (when (slot-boundp obj '%transform)
        (unless (eq (c-struct (transform obj)) tform)
          (free-later (c-struct (transform obj)))
          (setf (c-struct (transform obj)) tform)))
      (when (and (slot-boundp obj '%meshes)
                 (array-valid-p meshes (mesh-count obj) 'claylib/ll:mesh))
        (let ((mesh0 (i0 meshes 'claylib/ll:mesh)))
          (unless (eq (c-struct (meshes obj)) mesh0)
            (free-later (c-struct (meshes obj)))
            (setf (c-struct (meshes obj)) mesh0))))
      (when (and (slot-boundp obj '%materials)
                 (array-valid-p mats (material-count obj) 'claylib/ll:material))
        (let ((mat0 (i0 mats 'claylib/ll:material)))
          (unless (eq (c-struct (materials obj)) mat0)
            (free-later (c-struct (materials obj)))
            (setf (c-struct (materials obj)) mat0))))
      (when (and (slot-boundp obj '%bones)
                 (array-valid-p bones (bone-count obj) 'claylib/ll:bone-info))
        (let ((bone0 (i0 bones 'claylib/ll:bone-info)))
          (unless (eq (c-struct (bones obj)) bone0)
            (free-later (c-struct (bones obj)))
            (setf (c-struct (bones obj)) bone0))))
      (when (slot-boundp obj '%bind-pose)
        (unless (eq (c-struct (bind-pose obj)) bpose)
          (free-later (c-struct (bind-pose obj)))
          (setf (c-struct (bind-pose obj)) bpose)))))
  (when (slot-boundp obj '%materials)
    (sync-children (materials obj)))
  (when (slot-boundp obj '%bind-pose)
    (sync-children (bind-pose obj))))

(definitializer rl-model
  :lisp-slots ((%meshes) (%materials) (%bones) (%bind-pose) (%animations))
  :struct-slots ((%transform))
  :pt-accessors ((mesh-count integer)
                 (material-count integer)
                 (mesh-materials sequence)
                 (bone-count integer)))

(default-free rl-model %transform %meshes %materials %bones %bind-pose)
(default-free-c claylib/ll:model unload-model t)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
             :accessor asset))
    (:default-initargs
     :scale (make-vector3 1 1 1)
     :tint +white+
     :filled t)))

(definitializer model
  :lisp-slots ((%scale) (%tint) (%filled) (%asset)))

(defun make-model (model-asset x y z
                   &rest args &key scale tint rot-angle rot-axis filled
                                transform mesh-count material-count meshes
                                materials mesh-material bone-count bones
                                bind-pose animations)
  "Make a Claylib model.

Models are backed by RL-MODELs which draw reusable data from the given MODEL-ASSET."
  (declare (ignorable scale tint rot-angle rot-axis filled))
  (load-asset model-asset)
  (let ((model (apply #'make-instance 'model
                      :allow-other-keys t
                      :asset model-asset
                      :pos (make-vector3 x y z)
                      args))
        (rl-asset model-asset)
        (c-meshes (autowrap:c-aref (model.meshes (c-asset model-asset)) 0 'claylib/wrap:mesh))
        (c-bones (autowrap:c-aref (model.bones (c-asset model-asset)) 0 'claylib/wrap:bone-info))
        (c-materials (autowrap:c-aref (model.materials (c-asset model-asset)) 0 'claylib/wrap:material)))
    (set-slot :transform model (or transform (transform rl-asset))) ; TODO (make-zero-matrix) here?
    (set-slot :materials model (or materials (materials rl-asset)))
    (set-slot :bind-pose model (or bind-pose (bind-pose rl-asset)))
    (setf (mesh-count model) (or mesh-count (mesh-count rl-asset))
          (meshes model) (or meshes
                             (make-instance 'rl-meshes
                                            :cl-array (make-rl-*-array c-meshes
                                                                       (mesh-count model))))
          (material-count model) (or material-count (material-count rl-asset))
          (mesh-material model) (or mesh-material
                                    (make-instance 'rl-materials
                                                   :cl-array (make-rl-*-array c-materials
                                                                              (material-count model)))
                                    (mesh-material rl-asset))
          (bone-count model) (or bone-count (bone-count rl-asset))
          (bones model) (or bones
                            (make-instance 'rl-bones
                                           :cl-array (make-rl-*-array c-bones (bone-count model)))))
    model))

;(default-free model %scale %tint)
(defmethod free ((obj model))
  (dolist (slot '(%scale %tint %position %rot-axis %transform %meshes %materials %bones %bind-pose))
    (free (slot-value obj slot))
    (slot-makunbound obj slot)))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-struct obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))
