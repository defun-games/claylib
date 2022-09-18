(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-bone-info ()
    ((%c-struct
      :type claylib/ll:bone-info
      :initform (autowrap:alloc 'claylib/ll:bone-info)
      :accessor c-struct))))

(defcreader name rl-bone-info name bone-info)  ; TODO: Array/string
(defcreader parent rl-bone-info parent bone-info)

(defcwriter name rl-bone-info name bone-info string)  ; TODO: Array/string
(defcwriter parent rl-bone-info parent bone-info integer)

(definitializer rl-bone-info
  :pt-accessors ((name string)
                 (parent integer)))

(default-free rl-bone-info)
(default-free-c claylib/ll:bone-info)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model-animation ()
    ((%bones :initarg :bones
             :type rl-bone-info  ; TODO: Array/pointer
             :reader bones)
     (%frame-poses :initarg :frame-poses
                   :type rl-transform  ; TODO: Array/pointer-pointer
                   :reader frame-poses)
     (%c-struct
      :type claylib/ll:model-animation
      :initform (autowrap:alloc 'claylib/ll:model-animation)
      :accessor c-struct))))

(defcreader bone-count rl-model-animation bone-count model-animation)
(defcreader frame-count rl-model-animation frame-count model-animation)

(defcwriter bone-count rl-model-animation bone-count model-animation integer)
(defcwriter frame-count rl-model-animation frame-count model-animation integer)
(defcwriter-struct bones rl-model-animation bones model-animation bone-info name parent)  ; TODO: Array/pointer
(defcwriter-struct frame-poses
  rl-model-animation frame-poses model-animation transform translation rotation scale)

(defmethod sync-children ((obj rl-model-animation))
  (flet ((i0 (array type)
           (autowrap:c-aref array 0 type)))
    (unless (eq (c-struct (bones obj))
                (i0 (model-animation.bones (c-struct obj)) 'claylib/ll:bone-info))
      (free-later (c-struct (bones obj)))
      (setf (c-struct (bones obj))
            (i0 (model-animation.bones (c-struct obj)) 'claylib/ll:bone-info)))
    (unless (eq (c-struct (frame-poses obj))
                (i0 (model-animation.frame-poses (c-struct obj)) 'claylib/ll:transform))
      (free-later (c-struct (frame-poses obj)))
      (setf (c-struct (frame-poses obj))
            (i0 (model-animation.frame-poses (c-struct obj)) 'claylib/ll:transform)))
    (sync-children (frame-poses obj))))

(definitializer rl-model-animation
  :struct-slots ((%bones) (%frame-poses))
  :pt-accessors ((bone-count integer)
                 (frame-count integer)))

(default-free rl-model-animation %bones %frame-poses)

(defmethod free ((anim claylib/ll:model-animation))
  ;; TODO: Access the num somehow and use UNLOAD-MODEL-ANIMATIONS to unload all of them
  (when (autowrap:valid-p anim)
    (unload-model-animation anim)
    (autowrap:free anim)))
