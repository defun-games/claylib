(in-package #:claylib)

(defmacro defreader (slot type sub-reader sub-slot)
  "Define a pass-through slot reader for a Claylib CLOS object. Example usage:

(defreader width cube x size)

'To read the WIDTH value of a CUBE, read the X value of the SIZE slot of that cube.' SIZE is
another CLOS object, in this case a vector3. If SIZE is a C struct, you need DEFCREADER."
  (let ((obj (gensym)))
    `(defmethod ,slot ((,obj ,type))
       (,sub-reader (,sub-slot ,obj)))))

(defmacro defcreader (lisp-slot lisp-type c-slot c-type)
  "Define a pass-through slot reader for a Claylib CLOS object backed by a C struct.
Example usage:

(defcreader rot rl-camera-2d rotation camera2d)

'To read the ROT value of an RL-CAMERA-2D, read the ROTATION value of the CAMERA2D struct backing it.'
If CAMERA2D is a CLOS object, you need DEFREADER."
  (let ((obj (gensym))
        (c-reader (intern (format nil "~:@a.~:@a" c-type c-slot) 'claylib/ll)))
    `(defmethod ,lisp-slot ((,obj ,lisp-type))
       (,c-reader (c-struct ,obj)))))

(defmacro defcreader-bool (lisp-slot lisp-type c-slot c-type)
  "A version of DEFCREADER that also converts C booleans to Lisp booleans."
  (let ((obj (gensym))
        (c-reader (intern (format nil "~:@a.~:@a" c-type c-slot) 'claylib/ll)))
    `(defmethod ,lisp-slot ((,obj ,lisp-type))
       (= (,c-reader (c-struct ,obj)) 1))))

(defmacro defwriter (slot type sub-writer sub-slot &optional value-type)
  "Define a pass-through slot writer for a Claylib CLOS object. Example usage:

(defwriter width rectangle x size)

'To set the WIDTH value of a RECTANGLE, set the X value of the SIZE object backing it.
In this case, SIZE is a vector2. If SIZE is a C struct, you need DEFCWRITER or maybe even
DEFCWRITER-VEC."
  (let ((value (gensym))
        (obj (gensym)))
    `(defmethod (setf ,slot) (,(if value-type
                                   `(,value ,value-type)
                                   value)
                              (,obj ,type))
       (setf (,sub-writer (,sub-slot ,obj)) ,value))))

(defmacro defcwriter (lisp-slot lisp-type c-slot c-type &optional value-type coerce-type)
  "Define a pass-through slot writer for a Claylib CLOS object backed by a C struct.
Example usage:

(defcwriter rot rl-camera-2d rotation camera2d number float)

'To set the ROT value of an RL-CAMERA-2D, set the ROTATION value of the CAMERA2D struct backing it.
Oh yeah, and make sure it's a float.' If CAMERA2D is a CLOS object, you need DEFWRITER."
  (let ((value (gensym))
        (obj (gensym))
        (c-writer (intern (format nil "~:@a.~:@a" c-type c-slot) 'claylib/ll)))
    `(defmethod (setf ,lisp-slot) (,(if value-type
                                        `(,value ,value-type)
                                        value)
                                   (,obj ,lisp-type))
       (setf (,c-writer (c-struct ,obj))
             ,(if coerce-type
                  `(coerce ,value ',coerce-type)
                  value)))))

(defmacro defcwriter-bool (lisp-slot lisp-type c-slot c-type)
  "A version of DEFCWRITER that converts Lisp booleans to C booleans."
  (let ((value (gensym))
        (obj (gensym))
        (c-writer (intern (format nil "~:@a.~:@a" c-type c-slot) 'claylib/ll)))
    `(defmethod (setf ,lisp-slot) (,value (,obj ,lisp-type))
       (check-type ,value boolean)
       (setf (,c-writer (c-struct ,obj))
             (if ,value
                 1
                 0)))))

(defmacro defwriter-float (writer-name type &optional slot-name)
  "This just defines a simple slot writer to coerce the written value into a float.
For something more complex, try DEFWRITER or DEFCWRITER."
  (let ((value (gensym))
        (obj (gensym))
        (%slot (intern (format nil "%~:@a" writer-name) 'claylib)))
    `(defmethod (setf ,writer-name) (,value (,obj ,type))
       (setf (slot-value ,obj ',(or slot-name %slot)) (coerce ,value 'float)))))

(defmacro defcwriter-struct (lisp-slot lisp-type c-slot c-type struct-type &rest readers)
  "This macro exists to work around a limitation of Autowrap, which is that we can't directly SETF an
entire struct -- we must set each field one by one. However, sometimes you need to make sure two
structs are actually identical, not just holding the same values. And some objects, like cameras,
are backed by multiple vectors, so this limitation gets annoying quickly. DEFCWRITER-STRUCT smoothes
this issue out.

DEFCWRITER-STRUCT defines a writer that basically does three things:

1. Sets every field of a CLOS object's C vector to the values of the CLOS vector the user passes in
2. Marks the backing C struct of the passed CLOS object to be freed later
3. Sets the CLOS object's backing C struct to the same one that was just mutated

Example usage:

(defcwriter-struct pos camera-3d position camera3d vector3)

'To set the POS value of a CAMERA-3D, set the POSITION value (a vector3) of the CAMERA3D struct
backing it.'"
  (let ((value (gensym))
        (obj (gensym))
        (slot (gensym))
        (c-writer (intern (format nil "~:@a.~:@a" c-type c-slot) 'claylib/ll))
        (struct-setter (intern (format nil "SET-~:@a" struct-type) 'claylib/ll)))
    `(defmethod set-slot ((,slot (eql ,(alexandria:make-keyword lisp-slot)))
                          (,obj ,lisp-type)
                          (,value ,(intern (format nil "RL-~:@a" struct-type)))
                          &key (free :later))
       (,struct-setter (,c-writer (c-struct ,obj))
                       ,@(loop for reader in readers
                               collect `(,reader ,value)))
       (case free
         (:now (free ,value))
         (:later (free-later ,value))
         (:never nil)
         (t (error ":FREE must be :NOW, :LATER, or :NEVER")))
       (handler-case (,lisp-slot ,obj)
         (unbound-slot ()
           (setf (slot-value ,obj ',(intern (format nil "%~:@a" lisp-slot))) ,value)))
       (setf (c-struct (,lisp-slot ,obj)) (,c-writer (c-struct ,obj))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rl-class-p (type)
    "Determine whether a type represents a Raylib 'mirror' class -- these classes begin with 'RL-' and
always have a direct '%C-STRUCT slot."
    (member-if #'(lambda (slot)
                   (eql (closer-mop:slot-definition-name slot) '%c-struct))
               (closer-mop:class-direct-slots (find-class type))))

  (defun rl-subclass-p (type)
    "Determine whether a type represents a Raylib 'mirror' class or subclass -- these classes begin
with 'RL-' and always have a direct or inherited '%C-STRUCT slot."
    (let ((class (find-class type)))
      (member-if #'(lambda (slot)
                     (eql (closer-mop:slot-definition-name slot) '%c-struct))
                 (handler-case
                     (closer-mop:class-slots class)
                   (error ()
                     (progn
                       ;; TODO: Kind of a hack but I'm not sure the best place to do this.
                       (closer-mop:finalize-inheritance class)
                       (closer-mop:class-slots class)))))))

  (defun expand-check-types (args &optional allow-null-p)
    "Expected format per ARG: (ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
    (loop for arg in args
          collect (destructuring-bind (accessor type &optional coerce-type default-value) arg
                    (declare (ignore coerce-type default-value))
                    `(check-type ,accessor ,(if allow-null-p
                                                `(or ,type null)
                                                type))))))

(defmacro definitializer (type &rest slots)
  "Define an initialize-instance :after method for a passed type. Each SLOT is expected to have
the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)

This macro adds type checking, finalizers where needed, and makes sure to use SET-SLOT for structs.
Note that you may pass NIL as COERCE-TYPE if you only want to use a default value without
type coercion."
  (let ((obj (gensym))
        (ptr (gensym)))
    `(defmethod initialize-instance :after ((,obj ,type)
                                            &key ,@(mapcar #'(lambda (slot)
                                                               (if (= (length slot) 4)
                                                                   `(,(car slot) ,(fourth slot))
                                                                   (car slot)))
                                                           slots))
       ,@(expand-check-types slots t)
       ,@(loop for slot in slots
               collect (destructuring-bind (accessor type &optional coerce-type default-value) slot
                         (declare (ignore default-value))
                         `(when (or ,(eql type 'boolean) ,accessor)
                            ,(if (and (subtypep type 'standard-object)
                                      (= (length slot) 2))
                                 `(set-slot ,(alexandria:make-keyword accessor) ,obj ,accessor)
                                 `(setf (,accessor ,obj)
                                        ,(if coerce-type
                                             `(coerce ,accessor ',coerce-type)
                                             accessor))))))
       (when (and (rl-class-p ',type)
                  (c-struct ,obj))
         ;; TODO: This probably needs a child wrapper check.
         (tg:finalize ,obj
                      (let ((,ptr (autowrap:ptr (c-struct ,obj))))
                        (lambda () (autowrap:free ,ptr))))))))

(defmacro definitializer-float (type &rest accessors)
  "Define a simple initialize-instance :after method to ensure that some number of slot values
are coerced to floats. For something more complex, try DEFINITIALIZER."
  (let ((obj (gensym)))
    `(defmethod initialize-instance :after ((,obj ,type) &key)
       ,@(loop for accessor in accessors
               collect `(when (integerp (,accessor ,obj))
                          (setf (,accessor ,obj) (coerce (,accessor ,obj) 'float)))))))

(defmacro default-free (type)
  "Define a FREE method that is sane for most Lisp types."
  (let ((obj (gensym)))
    `(defmethod free ((,obj ,type))
       (when (and (slot-exists-p ,obj '%c-struct)
                  (c-struct ,obj)
                  (autowrap:valid-p (c-struct ,obj)))
         (free (c-struct ,obj)))
       (when (slot-exists-p ,obj '%c-struct)
         (setf (slot-value ,obj '%c-struct) nil))
       (tg:cancel-finalization ,obj)
       (when (next-method-p)
         (call-next-method)))))

(defmacro default-free-c (type &optional fn window-required-p)
  "Define a FREE method that is sane for most C types."
  ;; TODO: Is there a better way of ensuring we don't free a child wrapper?
  (let ((validity-fn (intern (remove #\' (format nil "~:@a-VALIDITY" type)) 'claylib/wrap))
        (obj (gensym)))
    `(defmethod free ((,obj ,type))
       (when (eql (,validity-fn ,obj) t)
         ,(when fn
            `(when (or (and ,window-required-p (is-window-ready-p))
                       ,(and fn (not window-required-p)))
               (,fn ,obj)))
         (autowrap:free ,obj)))))

(defmacro default-slot-value (class slot-name value)
  "Define a SLOT-UNBOUND method as a lazy fallback default slot value."
  (let ((obj (gensym))
        (slot (gensym)))
    `(defmethod slot-unbound (_
                              (,obj ,class)
                              (,slot (eql ',slot-name)))
       (setf (slot-value ,obj ,slot) ,value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-c-fun-args (args)
    "Expected format per ARG: (ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
    (loop for arg in args
          collect (destructuring-bind (accessor type &optional coerce-type default-value) arg
                    (declare (ignore default-value))
                    (cond
                      (coerce-type `(coerce ,accessor ',coerce-type))
                      ((and (subtypep type 'standard-object)
                            (rl-subclass-p type))
                       `(c-struct ,accessor))
                      ((eql type 'boolean)
                       `(if ,accessor 1 0))
                      (t accessor))))))

(defmacro defun-pt (name c-fn docstring &rest args)
  "Define a 'pass-through' function. These functions are mirrored Raylib functions with minimal
massaging of arguments. Each ARG is expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
  `(defun ,name ,(remove nil `(,@(mapcar #'(lambda (arg)
                                             (unless (fourth arg)
                                               (car arg)))
                                  args)
                               &key ,@(mapcar #'(lambda (arg)
                                                  (when (fourth arg)
                                                    `(,(car arg) ,(fourth arg))))
                                              args)))
     ,docstring
     ,@(expand-check-types args)
     (,c-fn ,@(expand-c-fun-args args))
     ,(caar args)))

(defmacro defun-pt-bool (name c-fn docstring &rest args)
  "Define a special 'pass-through' function for which the return value is a boolean value. Each ARG
is expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
  `(defun ,name ,(remove nil `(,@(mapcar #'(lambda (arg)
                                             (unless (fourth arg)
                                               (car arg)))
                                  args)
                               &key ,@(mapcar #'(lambda (arg)
                                                  (when (fourth arg)
                                                    `(,(car arg) ,(fourth arg))))
                                              args)))
     ,docstring
     ,@(expand-check-types args)
     (if (= 0 (,c-fn ,@(expand-c-fun-args args)))
         nil
         t)))

(defmacro defun-pt-void (name c-fn docstring &rest args)
  "Define a special 'pass-through' function for which the C function does not return anything. Each
ARG is expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
  `(defun ,name ,(remove nil `(,@(mapcar #'(lambda (arg)
                                             (unless (fourth arg)
                                               (car arg)))
                                  args)
                               &key ,@(mapcar #'(lambda (arg)
                                                  (when (fourth arg)
                                                    `(,(car arg) ,(fourth arg))))
                                              args)))
     ,docstring
     ,@(expand-check-types args)
     (,c-fn ,@(expand-c-fun-args args))
     nil))

(defmacro defun-pt-num (name c-fn docstring &rest args)
  "Define a special 'pass-through' function that does not alter the C function's return value. Usually
appropriate when you are expecting a number (or string). Each ARG is expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE"
  `(defun ,name ,(remove nil `(,@(mapcar #'(lambda (arg)
                                             (unless (fourth arg)
                                               (car arg)))
                                  args)
                               &key ,@(mapcar #'(lambda (arg)
                                                  (when (fourth arg)
                                                    `(,(car arg) ,(fourth arg))))
                                              args)))
     ,docstring
     ,@(expand-check-types args)
     (,c-fn ,@(expand-c-fun-args args))))

(defmacro defun-pt-arg0 (name c-fn allocate-form docstring &rest args)
  "Define a special 'pass-through' function in which the first argument is destructively modified
unless ALLOCATE-P is T. ALLOCATE-FORM is the form that is evaluated in the latter case. Each ARG is
expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
  `(defun ,name (,@(mapcar #'car args) &optional allocate-p)
     ,docstring
     ,@(expand-check-types args)
     (check-type allocate-p boolean)
     (let ((retval (if allocate-p ,allocate-form ,(caar args))))
       (,c-fn (c-struct retval)
              ,@(expand-c-fun-args args))
       retval)))
