(in-package #:claylib)

(defclass alloc-pool ()
  ((%objtype :initarg :objtype
             :type (or symbol null)
             :reader objtype)
   (%constructor :initarg :constructor
                 :type (or function null)
                 :reader constructor)
   (%objects
    :initform (make-hash-table)
    :type hash-table
    :reader objects))
  (:documentation "Simple pool to hold pre-allocated named objects of a single type or mixed types."))

(defun set-alloc (name pool &optional type-or-constructor)
  "Allocate a new object, give it a NAME, and store it in POOL. If the pool does not have a default
OBJTYPE or CONSTRUCTOR, then TYPE-OR-CONSTRUCTOR must be specified."
  (check-type pool alloc-pool)
  (setf (gethash name (objects pool))
        (cond
          ((typep type-or-constructor 'function)
           (funcall type-or-constructor))
          ((and type-or-constructor
                (typep type-or-constructor 'symbol))
           (make-instance type-or-constructor))
          ((constructor pool)
           (funcall (constructor pool)))
          ((objtype pool)
           (make-instance (objtype pool)))
          (t (error "Don't know how to make NAME, you must specify OBJTYPE or CONSTRUCTOR.")))))

(defun get-alloc (name pool &optional type-or-constructor)
  "Get object named NAME from POOL. If the object does not exist it will be created, in which case
TYPE-OR-CONSTRUCTOR is passed to SET-ALLOC."
  (check-type pool alloc-pool)
  (alexandria:if-let ((obj (gethash name (objects pool))))
    obj
    (set-alloc name pool type-or-constructor)))

(defun rem-alloc (name pool)
  "Delete object NAME from POOL."
  (check-type pool alloc-pool)
  (remhash name (objects pool)))

(defmethod initialize-instance :after ((obj alloc-pool)
                                       &key names objtype constructor &allow-other-keys)
  (dolist (name names)
    (set-alloc name obj (or constructor objtype))))

(defun make-alloc-pool (&key names objtype constructor)
  "Create an allocation pool, with optional starting NAMES. OBJTYPE is a default type for the pool,
e.g. 'vector2. Will be created with MAKE-INSTANCE unless CONSTRUCTOR is passed. CONSTRUCTOR is a
function that takes no arguments. If NAMES is passed, then OBJTYPE or CONSTRUCTOR is required.

Note that no type checking is done on the objects. OBJTYPE/CONSTRUCTOR are purely for convenience."
  (make-instance 'alloc-pool :names names :objtype objtype :constructor constructor))

(defun alloc-pool (obj)
  (alexandria:if-let ((pool (slot-value obj '%alloc-pool)))
    pool
    (setf (slot-value obj '%alloc-pool) (make-alloc-pool))))
