(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass c-struct ()
    ((%c-ptr
      :type cffi:foreign-pointer
      :reader c-ptr))))

(defmethod initialize-instance :after ((obj c-struct) &rest initargs &key &allow-other-keys)
  (unless (slot-boundp obj '%c-ptr)
    (setf (slot-value obj '%c-ptr) (getf initargs :c-ptr)))
  (tg:finalize obj
               (let ((ptr (c-ptr obj)))
                 (lambda () (cffi:foreign-free ptr)))))

(define-print-object c-struct
  ())
