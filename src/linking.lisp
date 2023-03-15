(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass linkable ()
    ((%children
      :initform (make-hash-table)
      :type hash-table
      :reader children))
    (:documentation "Add a %children slot for linking purposes. Any potentially drawn class containing
a numerical writer should inherit from this class.")))

(define-print-object linkable
  (children))

(defun link-objects (parent writer &rest children)
  "Link a PARENT's WRITER with CHILDREN so that children will be modified whenever the parent is.
Each child is a list of the form (CHILD-OBJECT CHILD-WRITER TRIGGER). TRIGGER is one of...

:SETF -- Call SETF on the designated child writer to set it to the same value as the parent.

:INCF -- Acts just like normal INCF by incrementing/decrementing the child by the same amount.

:SCALE -- Proportionally scales the child value by the same multiplier as the parent.

A function -- You can also pass an arbitrary function that takes five arguments:
PARENT-WRITER, PARENT-OBJ, VALUE, CHILD-WRITER, CHILD-OBJ. Unlike the others, VALUE here
does not have to be numeric."
  (let ((kids (gethash writer (children parent))))
    (dolist (child children) (pushnew child kids))
    (setf (gethash writer (children parent)) kids)))

(defun unlink-objects (parent writer &rest children)
  "Unlink a PARENT's WRITER from CHILDREN. Each child is a cons of the form (OBJECT . WRITER)."
  (setf (gethash writer (children parent))
        (remove-if #'(lambda (child)
                       (find child children :test #'(lambda (a b)
                                                      (and (equal (car a) (car b))
                                                           (equal (cadr a) (cdr b))))))
                   (gethash writer (children parent)))))

(defun set-linked-children (writer obj value)
  "Check if writer+object has any linked children, and call the corresponding writers via INCF."
  (alexandria:when-let ((children (gethash writer (children obj))))
    (mapcar #'(lambda (child)
                (destructuring-bind (cobj cwriter trigger) child
                  (case trigger
                    (:setf (funcall (fdefinition `(setf ,cwriter)) value cobj))
                    (:incf (funcall (fdefinition `(setf ,cwriter))
                                    (+ (funcall (fdefinition cwriter) cobj)
                                       (- value (funcall (fdefinition writer) obj)))
                                    cobj))
                    (:scale (let ((v (funcall (fdefinition writer) obj)))
                              (unless (= v 0)
                                (funcall (fdefinition `(setf ,cwriter))
                                         (* (funcall (fdefinition cwriter) cobj)
                                            (/ value v))
                                         cobj))))
                    (t (funcall trigger writer obj value cwriter cobj)))))
            children)))
