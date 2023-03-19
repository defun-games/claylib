(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass c-ptr ()
    ((%c-ptr
      :type cffi:foreign-pointer))
    (:default-initargs
     :finalize t)
    (:documentation
     "A 1:1 mapping between Lisp and C objects, meant to make memory management easier.
This is an analogue of cl-autowrap's wrapper objects."))

  (defclass c-struct ()
    ((%c-ptr
      :type c-ptr))
    (:documentation "Parent class of RL-* mirror classes.")))

(defmethod initialize-instance :after ((obj c-ptr) &rest initargs &key &allow-other-keys)
  (unless (slot-boundp obj '%c-ptr)
    (setf (slot-value obj '%c-ptr) (getf initargs :c-ptr)))
  (when (getf initargs :finalize)
    (tg:finalize obj
                 (let ((ptr (slot-value obj '%c-ptr)))
                   (lambda () (cffi:foreign-free ptr))))))

(defmethod initialize-instance :after ((obj c-struct) &rest initargs &key &allow-other-keys)
  (unless (slot-boundp obj '%c-ptr)
    (setf (slot-value obj '%c-ptr) (make-instance 'c-ptr
                                                  :c-ptr (getf initargs :c-ptr)
                                                  :finalize (getf initargs :finalize)))))

(defun c-ptr (c-struct)
  (check-type c-struct c-struct)
  (slot-value (slot-value c-struct '%c-ptr) '%c-ptr))

(defun set-c-ptr (c-struct value)
  (check-type c-struct c-struct)
  (check-type value c-ptr)
  (setf (slot-value c-struct '%c-ptr) value))

(defsetf c-ptr set-c-ptr)

(define-print-object c-ptr
    (c-ptr))

(define-print-object c-struct
    (c-ptr))
