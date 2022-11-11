(in-package #:claylib)

(default-unload claylib/ll:model unload-model t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model ()
    ((%transform :initarg :transform
                 :type rl-matrix
                 :reader transform)
     (%meshes :initarg :meshes
              :type rl-meshes
              :accessor meshes)
     (%materials :initarg :materials
                 :type rl-materials
                 :accessor materials)
     (%bones :initarg :bones
             :type rl-bones
             :accessor bones)
     (%bind-pose :initarg :bind-pose
                 :type rl-transforms
                 :accessor bind-pose)
     (%animations :initarg :animations
                  :type rl-animations
                  :accessor animations)
     (%c-struct
      :type claylib/ll:model
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:model))))

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

;; Define SETF :AFTER methods to update the Raylib Model array pointers when setting any rl-sequence
;; slot in a Claylib model (e.g. %meshes)

(defmethod (setf meshes) :after ((value rl-meshes) (model rl-model))
  (setf (model.meshes (c-struct model)) (autowrap:ptr (c-struct (elt value 0)))))

(defmethod (setf materials) :after ((value rl-materials) (model rl-model))
  (setf (model.materials (c-struct model)) (autowrap:ptr (c-struct (elt value 0)))))

(defmethod (setf bones) :after ((value rl-bones) (model rl-model))
  (setf (model.bones (c-struct model)) (autowrap:ptr (c-struct (elt value 0)))))

(defmethod (setf bind-pose) :after ((value rl-transforms) (model rl-model))
  (setf (model.bind-pose (c-struct model)) (autowrap:ptr (c-struct (elt value 0)))))



(definitializer rl-model
  :lisp-slots ((%meshes) (%materials) (%bones) (%bind-pose) (%animations))
  :struct-slots ((%transform))
  :pt-accessors ((mesh-count integer)
                 (material-count integer)
                 (mesh-materials sequence)
                 (bone-count integer)))



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
                   &rest args &key scale tint rot-angle rot-axis filled transform mesh-count
                                material-count meshes materials mesh-materials bone-count bones
                                bind-pose animation-asset)
  "Make a Claylib model.

Models are backed by RL-MODELs which draw reusable data from the given MODEL-ASSET."
  (declare (ignorable scale tint rot-angle rot-axis filled))
  (load-asset model-asset)
  (when animation-asset (load-asset animation-asset))
  (let ((model (apply #'make-instance 'model
                      :allow-other-keys t
                      :asset model-asset
                      :pos (make-vector3 x y z)
                      args))
        (c-meshes (autowrap:c-aref (model.meshes (c-asset model-asset)) 0 'claylib/ll:mesh))
        (c-bones (autowrap:c-aref (model.bones (c-asset model-asset)) 0 'claylib/ll:bone-info))
        (c-materials (autowrap:c-aref (model.materials (c-asset model-asset)) 0 'claylib/ll:material))
        (c-poses (autowrap:c-aref (model.bind-pose (c-asset model-asset)) 0 'claylib/ll:transform)))
    (set-slot :transform model (or transform (transform model-asset))) ; TODO (make-zero-matrix) here?
    (setf (mesh-count model)
          (or mesh-count (mesh-count model-asset))

          (meshes model)
          (or meshes (make-instance 'rl-meshes
                                    :cl-array (make-rl-*-array c-meshes (mesh-count model))))
          (material-count model)
          (or material-count (material-count model-asset))

          (materials model)
          (or materials (make-instance 'rl-materials
                                       :cl-array (make-rl-*-array c-materials
                                                                  (material-count model))))

          (mesh-materials model)
          (or mesh-materials (mesh-materials model-asset))

          (bone-count model)
          (or bone-count (bone-count model-asset))

          (bones model)
          (or bones (make-instance 'rl-bones
                                   :cl-array (make-rl-*-array c-bones (bone-count model))))

          (bind-pose model)
          (or bind-pose (make-instance 'rl-transforms
                                       ;; TODO Is bone-count the right number of bind-poses?
                                       :cl-array (make-rl-*-array c-poses (bone-count model)))))
    (when animation-asset
      (setf (animations model) (asset animation-asset)))
    model))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-struct obj)
                            (c-struct (pos obj))
                            (c-struct (rot-axis obj))
                            (rot-angle obj)
                            (c-struct (scale obj))
                            (c-struct (tint obj))))
