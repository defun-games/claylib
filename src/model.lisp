(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model (c-struct linkable)
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
                  :accessor animations))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:model))))

(defcreader mesh-count rl-model mesh-count model)
(defcreader material-count rl-model material-count model)
(defcreader bone-count rl-model bone-count model)
(defmethod mesh-material ((model rl-model) (index integer))
  (when (and (< index (mesh-count model))
             (>= index 0))
    (cffi:mem-aref (field-value (c-ptr model) 'model 'mesh-material)
                   :int
                   index)))
(defmethod mesh-materials ((model rl-model))
  (loop for i below (mesh-count model)
        collect (mesh-material model i)))

(define-print-object rl-model
    (transform meshes materials bones bind-pose animations mesh-count material-count bone-count mesh-materials))

(child-setter rl-model meshes materials bones bind-pose animations)

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
    (claylib/ll:set-model-mesh-material (c-ptr model) index value)))
(defmethod (setf mesh-materials) ((value sequence) (model rl-model))
  (when (cffi:null-pointer-p (field-value (c-ptr model) 'model 'mesh-material))
    (setf (field-value (c-ptr model) 'model 'mesh-material)
          (calloc :int (length value))))
  (dotimes (i (mesh-count model))
    (setf (mesh-material model i) (if (< i (length value))
                                      (elt value i)
                                      0))))

;; Define SETF :AFTER methods to update the Raylib Model array pointers when setting any rl-sequence
;; slot in a Claylib model (e.g. %meshes)

(defmethod (setf meshes) :after ((value rl-meshes) (model rl-model))
  (setf (field-value (c-ptr model) 'model 'meshes)
        (c-ptr (elt value 0))))

(defmethod (setf materials) :after ((value rl-materials) (model rl-model))
  (setf (field-value (c-ptr model) 'model 'materials)
        (c-ptr (elt value 0))))

(defmethod (setf bones) :after ((value rl-bones) (model rl-model))
  (setf (field-value (c-ptr model) 'model 'bones)
        (c-ptr (elt value 0))))

(defmethod (setf bind-pose) :after ((value rl-transforms) (model rl-model))
  (setf (field-value (c-ptr model) 'model 'bind-pose)
        (c-ptr (elt value 0))))



(definitializer rl-model
  :lisp-slots ((%meshes) (%materials) (%bones) (%bind-pose) (%animations))
  :struct-slots ((%transform))
  :pt-accessors ((mesh-count integer)
                 (material-count integer)
                 (mesh-materials sequence)
                 (bone-count integer))
  :unload (safe-unload-model t))



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
             :accessor asset)
     (%bbox
      :initform nil
      :type (or rl-bounding-box null)
      :reader bbox))
    (:default-initargs
     :scale (make-vector3 1 1 1)
     :tint +white+
     :filled t)))

(child-setter model scale tint filled asset)

(define-print-object model
    (scale tint filled asset bbox))

(definitializer model
  :lisp-slots ((%scale) (%tint) (%filled) (%asset) (%bbox)))

(defun make-model (model-asset x y z
                   &rest args &key scale tint rot-angle rot-axis filled transform mesh-count
                                meshes material-count materials mesh-materials bone-count bones
                                bind-pose animation-asset instance-meshes-p instance-materials-p
                                bounding-box-p)
  "Make a Claylib model.

Models are backed by RL-MODELs which draw reusable data from the given MODEL-ASSET."
  (declare (ignorable scale tint rot-angle rot-axis filled))
  (load-asset model-asset)
  (when animation-asset (load-asset animation-asset))
  (let* ((c-model (c-asset model-asset))
         (model (apply #'make-instance 'model
                       :allow-other-keys t
                       :asset model-asset
                       :pos (make-vector3 x y z)
                       :c-ptr (partial-copy-model
                               c-model
                               ;; Copy transforms and counts from the underlying asset
                               (list (unless transform 'claylib/ll::transform)
                                     (unless mesh-count 'claylib/ll::mesh-count)
                                     (unless material-count 'claylib/ll::material-count)
                                     (unless bone-count 'claylib/ll::bone-count))
                               ;; Reuse the asset's bones and bind pose
                               (list (unless bones 'claylib/ll::bones)
                                     (unless bind-pose 'claylib/ll::bind-pose)))
                       args))
         (c-meshes (field-value c-model 'model 'meshes))
         (c-bones (cffi:mem-aref (field-value c-model 'model 'bones) 'claylib/ll:bone-info))
         (c-materials (field-value c-model 'model 'materials))
         (c-poses (cffi:mem-aref (field-value c-model 'model 'bind-pose) 'claylib/ll:transform)))
    (when transform (set-slot :transform (transform model) transform))
    (when mesh-count (setf (mesh-count model) mesh-count))
    (when material-count (setf (material-count model) material-count))
    (when bone-count (setf (bone-count model) bone-count))

    ;; Create meshes and materials Lisp arrays based on the C data
    (setf (meshes model)
          (or meshes (make-instance 'rl-meshes
                                    :cl-array (make-rl-mesh-array
                                               (if instance-meshes-p
                                                   (cffi:mem-aref c-meshes 'claylib/ll:mesh)
                                                   (copy-c-array 'claylib/ll:mesh
                                                                 c-meshes
                                                                 (mesh-count model)))
                                               (mesh-count model))))

          (materials model)
          (or materials
              (make-instance 'rl-materials
                             :cl-array (make-rl-material-array
                                        (if instance-materials-p
                                            (cffi:mem-aref c-materials 'claylib/ll:material)
                                            (let ((c-array
                                                    (calloc 'claylib/ll:material
                                                            (material-count model))))
                                              (dotimes (i (material-count model))
                                                (full-copy-material (cffi:mem-aref c-materials
                                                                                   'claylib/ll:material
                                                                                   i)
                                                                    (cffi:mem-aref c-array
                                                                                   'claylib/ll:material
                                                                                   i)))
                                              c-array))
                                        (material-count model)))))

    ;; Same story with animation data, if relevant
    (unless (= 0 (bone-count model))
      (setf (bones model)
            (or bones
                (make-instance 'rl-bones
                               :cl-array (make-rl-bone-info-array c-bones (bone-count model))))

            (bind-pose model)
            (or bind-pose
                (make-instance 'rl-transforms
                               :cl-array (make-rl-transform-array c-poses (bone-count model))))))

    ;; Same with mesh-material mappings, if materials are not instanced
    (cond
      (mesh-materials
       (setf (mesh-materials model) mesh-materials))
      (instance-materials-p
       (setf (mesh-materials model) (mesh-materials model-asset)))
      (t
       (let ((mm (calloc :int (mesh-count model))))
         (copy-c-array :int
                       (field-value c-model 'model 'mesh-material)
                       (mesh-count model)
                       mm)
         (setf (field-value (c-ptr model) 'model 'mesh-material) mm))))

    (when animation-asset
      (setf (animations model) (asset animation-asset)))

    ;; For non-instanced meshes we must reset the VAO and VBO ID's or GL gets very upset.
    (unless (or instance-meshes-p meshes)
      (dotimes (i (length (meshes model)))
        (let ((c-mesh (c-ptr (elt (meshes model) i))))
          (setf (field-value c-mesh 'mesh 'vao-id) 0
                (field-value c-mesh 'mesh 'vbo-id) (calloc :unsigned-int 7))
          (upload-mesh c-mesh 0))))

    ;; Calculate a bounding box and keep it updated along with the model
    (when bounding-box-p
      (setf (slot-value model '%bbox)
            (get-model-bounding-box model :bounding-box (make-instance 'bounding-box)))
      (let ((new-bbox #'(lambda (pwriter pobj value cwriter cobj)
                          (declare (ignore pwriter value cwriter))
                          (get-model-bounding-box pobj :bounding-box cobj))))
        (dolist (writer '(x y z))
          (link-objects model writer
                        (list (low (bbox model)) writer :incf)
                        (list (high (bbox model)) writer :incf))
          (link-objects (scale model) writer
                        (list (low (bbox model)) writer :scale)
                        (list (high (bbox model)) writer :scale))
          (link-objects (rot-axis model) writer (list (bbox model) nil new-bbox)))
        (link-objects model 'rot-angle (list (bbox model) nil new-bbox))))
    model))

(defmethod draw-object ((obj model))
  (claylib/ll:draw-model-ex (c-ptr obj)
                            (c-ptr (pos obj))
                            (c-ptr (rot-axis obj))
                            (rot-angle obj)
                            (c-ptr (scale obj))
                            (c-ptr (tint obj))))

(static-draw draw-model-object model)
