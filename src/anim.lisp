(in-package #:claylib)

(defclass rl-bone-info ()
  ((%c-struct
    :type claylib/ll:bone-info
    :initform (autowrap:alloc 'claylib/ll:bone-info)
    :accessor c-struct)))

(defcreader name rl-bone-info name bone-info)
(defcreader parent rl-bone-info parent bone-info)

(defcwriter name rl-bone-info name bone-info string)
(defcwriter parent rl-bone-info parent bone-info integer)

(definitializer rl-bone-info (name string) (parent integer))

(default-free rl-bone-info)
(default-free-c claylib/ll:bone-info)



(defclass rl-model-animation ()
  ((%bones :initarg :bones
           :type rl-bone-info  ; pointer
           :reader bones)
   (%frame-poses :initarg :frame-poses
                 :type rl-transform  ; pointer-pointer
                 :reader frame-poses)
   (%c-struct
    :type claylib/ll:model-animation
    :initform (autowrap:alloc 'claylib/ll:model-animation)
    :accessor c-struct)))

(defcreader bone-count rl-model-animation bone-count model-animation)
(defcreader frame-count rl-model-animation frame-count model-animation)

(defcwriter bone-count rl-model-animation bone-count model-animation integer)
(defcwriter frame-count rl-model-animation frame-count model-animation integer)
(defcwriter-struct bones rl-model-animation bones model-animation bone-info parent)

(definitializer rl-model-animation
    (bone-count integer) (frame-count integer) (bones rl-bone-info) (frame-poses rl-transform))

(default-free rl-model-animation)

(defmethod free ((anim rl-model-animation))
  ;; TODO: Access the num somehow and use UNLOAD-MODEL-ANIMATIONS to unload all of them
  (when (autowrap:valid-p anim)
    (unload-model-animation anim)
    (autowrap:free anim)))
