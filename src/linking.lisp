(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass linkable ()
    ((%children
      :initform (make-hash-table)
      :type hash-table
      :reader children))
    (:documentation "Add a %children slot for linking purposes. Any potentially drawn class containing
a numerical writer should inherit from this class.")))

(defun link-objects (parent writer &rest children)
  "Link a PARENT's WRITER with CHILDREN so that children will be modified whenever the parent is.
Numerical values only. Uses INCF, not SETF. Each child is a cons of the form (OBJECT . WRITER)."
  (let ((kids (gethash writer (children parent))))
    (dolist (child children) (pushnew child kids))
    (setf (gethash writer (children parent)) kids)))

(defun unlink-objects (parent writer &rest children)
  "Unlink a PARENT's WRITER from CHILDREN. Each child is a cons of the form (OBJECT . WRITER)."
  (setf (gethash writer (children parent))
        (remove-if #'(lambda (child)
                       (member child children :test #'equal))
                   (gethash writer (children parent)))))

(defun set-linked-children (writer obj value)
  "Check if writer+object has any linked children, and call the corresponding writers via INCF."
  (alexandria:when-let ((children (gethash writer (children obj))))
    (mapcar #'(lambda (child)
                (funcall (fdefinition `(setf ,(cdr child)))
                         (+ (funcall (fdefinition (cdr child)) (car child))
                            (- value (funcall (fdefinition writer) obj)))
                         (car child)))
            children)))
