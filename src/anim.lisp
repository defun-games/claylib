(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-bone-info (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:bone-info))))

(defcreader name rl-bone-info name bone-info)  ; TODO: Array/string
(defcreader parent rl-bone-info parent bone-info)

(define-print-object rl-bone-info
    (name parent))

(defcwriter name rl-bone-info name bone-info string)  ; TODO: Array/string
(defcwriter parent rl-bone-info parent bone-info integer)

(definitializer rl-bone-info
  :pt-accessors ((name string)
                 (parent integer)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bone (rl-bone-info) ()))

(define-print-object bone
    ())


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-model-animation (c-struct linkable)
    ((%bones :initarg :bones
             :type rl-bones
             :reader bones)
     (%frame-poses :initarg :frame-poses
                   :type (array rl-transforms)
                   :reader frame-poses))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:model-animation))))

(defcreader bone-count rl-model-animation bone-count model-animation)
(defcreader frame-count rl-model-animation frame-count model-animation)

(define-print-object rl-model-animation
    (bones frame-poses bone-count frame-count))

(defcwriter bone-count rl-model-animation bone-count model-animation integer)
(defcwriter frame-count rl-model-animation frame-count model-animation integer)

(definitializer rl-model-animation
  :lisp-slots (#|(%bones) (%frame-poses)|#) ; TODO investigate what to do with these
  :pt-accessors ((bone-count integer)
                 (frame-count integer))
  :unload (unload-model-animation t))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass model-animation (rl-model-animation) ()))

(define-print-object model-animation
    ())


(defconstant +foreign-bone-info-size+ (cffi:foreign-type-size 'claylib/ll:bone-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-bones (rl-sequence)
    ((%cl-array :type (array rl-bone-info 1)))))

(define-print-object rl-bones
    ())

(defun make-rl-bone-info-array (c-ptr num &optional finalize)
  (let ((contents
          (loop for i below num
                collect (make-instance 'rl-bone-info
                                       :c-ptr (cffi:mem-aref c-ptr 'claylib/ll:bone-info i)
                                       :finalize (when finalize (= i 0))))))
    (make-array num
                :element-type 'rl-bone-info
                :initial-contents contents)))

(defmethod (setf sequences:elt) (value (sequence rl-bones) index)
  (check-type value rl-bone-info)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-bone-info-size+
                        :void))



(defconstant +foreign-animation-size+ (cffi:foreign-type-size 'claylib/ll:model-animation))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-animations (rl-sequence)
    ((%cl-array :type (array rl-model-animation 1)))))

(define-print-object rl-animations
    ())

(defun make-rl-model-animation-array (c-ptr num &optional finalize)
  (make-array
   num
   :element-type 'rl-model-animation
   :initial-contents
   (loop for i below num
         for c-elt = (cffi:mem-aref c-ptr 'claylib/ll:model-animation i)
         for anim = (make-instance 'rl-model-animation :c-ptr c-elt
                                                       :finalize (when finalize (= i 0)))
         for c-bones = (cffi:mem-ref (field-ptr c-elt 'model-animation 'bones) :pointer)
         for bone-count = (field-value c-elt 'model-animation 'bone-count)
         for frame-count = (field-value c-elt 'model-animation 'frame-count)
         do (setf
             (slot-value anim '%bones)
             (make-instance 'rl-bones
                            :cl-array (make-rl-bone-info-array c-bones bone-count))

             ;; Sadly, this is a 1D lisp array of rl-transforms arrays (not proper 2D array) so
             ;; we don't get to take advantage of aref. But this is probably not a user-facing
             ;; issue.
             (slot-value anim '%frame-poses)
             (make-array frame-count
                         :element-type 'rl-transforms
                         :initial-contents
                         (loop
                           with fp = (cffi:mem-ref
                                      (field-ptr c-elt 'model-animation 'frame-poses)
                                      :pointer)
                           for i below frame-count
                           ;; Dereference the Transform double pointer
                           for p = (cffi:mem-aref fp :pointer i)
                           collect (make-instance 'rl-transforms
                                                  :cl-array (make-rl-transform-array p bone-count)))))
         collect anim)))

(defmethod (setf sequences:elt) (value (sequence rl-animations) index)
  (check-type value rl-model-animation)
  (cffi:foreign-funcall "memcpy"
                        :pointer (c-ptr (elt sequence index))
                        :pointer (c-ptr value)
                        :int +foreign-animation-size+
                        :void))
