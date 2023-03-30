(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-bounding-box (linkable c-struct)
    ((%low :initarg :low
           :type rl-vector3
           :reader low)
     (%high :initarg :high
            :type rl-vector3
            :reader high))
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:bounding-box))))

(define-print-object rl-bounding-box
    (low high))

(defcwriter-struct low rl-bounding-box min bounding-box vector3 x y z)
(defcwriter-struct high rl-bounding-box max bounding-box vector3 x y z)

(definitializer rl-bounding-box
  :struct-slots ((%low nil min)
                 (%high nil max)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bounding-box (rl-bounding-box)
    ((%corners
      :initform (make-list 8 :initial-element (make-vector3 0 0 0))
      :type list
      :reader corners))))

(defmethod initialize-instance :after ((bbox bounding-box) &key &allow-other-keys)
  (let ((low (low bbox))
        (high (high bbox))
        (corners (corners bbox)))
    (dolist (n '(0 2 3 6))
      (link-objects low 'x (list (elt corners n) 'x :setf)))
    (dolist (n '(1 4 5 7))
      (link-objects high 'x (list (elt corners n) 'x :setf)))
    (dolist (n '(0 1 3 5))
      (link-objects low 'y (list (elt corners n) 'y :setf)))
    (dolist (n '(2 4 6 7))
      (link-objects high 'y (list (elt corners n) 'y :setf)))
    (dolist (n '(0 1 2 4))
      (link-objects low 'z (list (elt corners n) 'z :setf)))
    (dolist (n '(3 5 6 7))
      (link-objects high 'z (list (elt corners n) 'z :setf)))))
