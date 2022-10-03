(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model ()
    ((%transform :initarg :transform
                 :type rl-matrix
                 :reader transform)
     (%meshes :initarg :meshes
              :type rl-meshes
              :accessor meshes)
     (%materials :initarg :materials
                 ;; :type rl-materials  ; TODO: make rl-materials sequence type
                 :reader materials)
     (%bones :initarg :bones
             :type rl-bones
             :reader bones)
     (%bind-pose :initarg :bind-pose
                 :type rl-transform ; pointer
                 :reader bind-pose)
     (%c-struct
      :type claylib/ll:model
      :initform (autowrap:calloc 'claylib/ll:model)
      :accessor c-struct))))

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
(defcwriter-struct meshes rl-model meshes model mesh  ; TODO: Array/pointer
  vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors
  indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)
(defcwriter-struct materials rl-model materials model material  ; TODO: Array/pointer
  shader maps params)
(defcwriter-struct bones rl-model bones model bone-info ; TODO: Array/pointer
  name parent)
(defcwriter-struct bind-pose rl-model bind-pose model transform ; pointer
  trans rot scale)

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
  :struct-slots ((%transform) (%meshes) (%materials) (%bones) (%bind-pose))
  :pt-accessors ((mesh-count integer)
                 (material-count integer)
                 (mesh-material integer)
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
                                bind-pose)
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
        (c-bones (autowrap:c-aref (model.bones (c-asset model-asset)) 0 'claylib/wrap:bone-info)))
    (set-slot :transform model (or transform (transform rl-asset))) ; TODO (make-zero-matrix) here?
    (set-slot :materials model (or materials (materials rl-asset)))
    (set-slot :bind-pose model (or bind-pose (bind-pose rl-asset)))
    (setf (mesh-count model) (or mesh-count (mesh-count rl-asset))
          (meshes model) (or meshes
                             (make-instance 'rl-meshes
                                            :cl-array (make-meshes-array c-meshes
                                                                         (mesh-count model))))
          (material-count model) (or material-count (material-count rl-asset))
          (mesh-material model) (or mesh-material (mesh-material rl-asset))
          (bone-count model) (or bone-count (bone-count rl-asset))
          (bones model) (or bones
                            (make-instance 'rl-bones
                                           :cl-array (make-bones-array c-bones
                                                                       (bone-count model)))))
    ;; TODO: anims
    model))

(default-free model %scale %tint)

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-struct obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))
