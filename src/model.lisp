(in-package #:claylib)

(defclass rl-model ()
  ((%transform :initarg :transform
               :type rl-matrix
               :reader transform)
   (%meshes :initarg :meshes
            :type rl-meshes
            :accessor meshes)
   (%materials :initarg :materials
                                        ; TODO: make rl-materials sequence type
               :reader materials)
   (%bones :initarg :bones
                                        ; TODO: make rl-bones sequence type
           :reader bones)
   (%bind-pose :initarg :bind-pose
               :type rl-transform ; pointer
               :reader bind-pose)
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
;;
;; TODO make special writers that translate this into code:
;; When we set the meshes slot of an rl-model [to an rl-meshes object], set the meshes field in its
;; C struct to the pointer held in the %pointer slot of the rl-meshes object
;;
;; (defcwriter-struct meshes rl-model meshes model mesh ; pointer
;;   vertex-count triangle-count vertices texcoords texcoords2 normals tangents colors
;;   indices anim-vertices anim-normals bone-ids bone-weights vao-id vbo-id)
;; (defcwriter-struct materials rl-model materials model material ; pointer
;;   shader maps params)
;; (defcwriter-struct bones rl-model bones model bone-info ; pointer
;;   name parent)
;; (defcwriter-struct bind-pose rl-model bind-pose model transform ; pointer
;;   trans rot scale)

(definitializer rl-model
  (transform rl-matrix) (mesh-count integer) (material-count integer) (meshes rl-meshes)
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
          :accessor tint)))

(definitializer model (scale rl-vector3 nil) (tint rl-color nil))

(default-slot-value model %scale (make-vector3 1 1 1))
(default-slot-value model %tint +white+)

(defun make-model (model-asset x y z
                   &rest args &key scale tint rot-angle rot-axis)
  "Make a Claylib model.

Models are backed by RL-MODELs which draw reusable data from the given MODEL-ASSET."
  (declare (ignore scale tint rot-angle rot-axis))
  (load-asset model-asset)
  (let ((model (apply #'make-instance 'model
                      :pos (make-vector3 x y z)
                      args)))
    (set-model (c-struct model)
               (c-struct (make-zero-matrix))  ; fresh transform for each instance
               (mesh-count model-asset)
               (material-count model-asset)
               (model.meshes (c-asset model-asset))
               (model.materials (c-asset model-asset))
               (mesh-material model-asset)
               (bone-count model-asset)
               (model.bones (c-asset model-asset))
               (model.bind-pose (c-asset model-asset)))
    (setf (meshes model) (make-instance 'rl-meshes
                                        :c-struct (autowrap:c-aref
                                                   (model.meshes (c-asset model-asset))
                                                   0
                                                   'claylib/wrap:mesh)
                                        :mesh-count (mesh-count model-asset))
          ;; TODO make rl-materials
          ;; TODO make rl-bones
          ;; TODO make rl-bind-pose
          (mesh-count model) (mesh-count model-asset)
          (material-count model) (material-count model-asset)
          (bone-count model) (bone-count model-asset))
    model))

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
