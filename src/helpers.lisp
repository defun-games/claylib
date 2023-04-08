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

(defcreader rot rl-camera-2d rotation camera-2d)

'To read the ROT value of an RL-CAMERA-2D, read the ROTATION value of the CAMERA-2D struct backing it.'
If CAMERA-2D is a CLOS object, you need DEFREADER."
  (let ((obj (gensym)))
    `(defmethod ,lisp-slot ((,obj ,lisp-type))
       (field-value (c-ptr ,obj) ',c-type ',c-slot))))

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
       (set-linked-children ',slot ,obj ,value)
       (setf (,sub-writer (,sub-slot ,obj)) ,value))))

(defmacro defcwriter (lisp-slot lisp-type c-slot c-type &optional value-type coerce-type)
  "Define a pass-through slot writer for a Claylib CLOS object backed by a C struct.
Example usage:

(defcwriter rot rl-camera-2d rotation camera-2d number float)

'To set the ROT value of an RL-CAMERA-2D, set the ROTATION value of the CAMERA-2D struct backing it.
Oh yeah, and make sure it's a float.' If CAMERA-2D is a CLOS object, you need DEFWRITER."
  (let ((value (gensym))
        (obj (gensym)))
    `(defmethod (setf ,lisp-slot) (,(if value-type
                                        `(,value ,value-type)
                                        value)
                                   (,obj ,lisp-type))
       (set-linked-children ',lisp-slot ,obj ,value)
       (setf (field-value (c-ptr ,obj) ',c-type ',c-slot)
             ,(if coerce-type
                  `(coerce ,value ',coerce-type)
                  value)))))

(defmacro defwriter-float (writer-name type &optional slot-name)
  "This just defines a simple slot writer to coerce the written value into a float.
For something more complex, try DEFWRITER or DEFCWRITER."
  (let ((value (gensym))
        (obj (gensym))
        (%slot (intern (format nil "%~:@a" writer-name) 'claylib)))
    `(defmethod (setf ,writer-name) (,value (,obj ,type))
       (set-linked-children ',writer-name ,obj ,value)
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

(defcwriter-struct pos camera-3d position camera-3d vector3)

'To set the POS value of a CAMERA-3D, set the POSITION value (a vector3) of the CAMERA-3D struct
backing it.'"
  (let ((value (gensym))
        (obj (gensym))
        (slot (gensym))
        (struct-setter (intern (format nil "SET-~:@a" struct-type) 'claylib/ll)))
    `(defmethod set-slot ((,slot (eql ,(alexandria:make-keyword lisp-slot)))
                          (,obj ,lisp-type)
                          (,value ,(intern (format nil "RL-~:@a" struct-type))))
       (set-linked-children ',lisp-slot ,obj ,value)
       (,struct-setter (field-value (c-ptr ,obj) ',c-type ',c-slot)
                       ,@(loop for reader in readers
                               collect (if (listp reader)
                                           `(field-value (c-ptr ,value) ',(car reader) ',(cadr reader))
                                           `(,reader ,value))))
       (handler-case (,lisp-slot ,obj)
         (unbound-slot ()
           (setf (slot-value ,obj ',(intern (format nil "%~:@a" lisp-slot))) ,value)))
       (unless (cffi:pointer-eq (c-ptr (,lisp-slot ,obj))
                                (field-value (c-ptr ,obj) ',c-type ',c-slot))
         (setf (c-ptr (,lisp-slot ,obj))
               (make-instance 'c-ptr :finalize nil
                                     :c-ptr (field-value (c-ptr ,obj) ',c-type ',c-slot)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rl-subclass-p (type)
    "Determine whether a type represents a Raylib 'mirror' class or subclass -- these classes begin
with 'RL-' and always have a direct or inherited '%C-PTR slot."
    (let ((class (find-class type)))
      (member-if #'(lambda (slot)
                     (eql (closer-mop:slot-definition-name slot) '%c-ptr))
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

(defmacro definitializer (class &key lisp-slots struct-slots pt-accessors unload fn)
  "Define an initialize-instance :after method for a passed class name.

:lisp-slots is a list of CLOS slots that are *not* tied to a backing C struct. Expected format:
(SLOT-NAME &optional USE-WRITER-P COERCE-TYPE)
Both the latter default to NIL. Set USE-WRITER-P when there is a custom writer to be used instead
of the default slot setter. COERCE-TYPE will usually be float, when applicable.

:struct-slots is a list of CLOS slots tied to a backing C struct. These slots themselves will
contain other CLOS objects. Expected format:
(SLOT-NAME &optional SUBCLASS C-FIELD)
Set SUBCLASS when you want to initialize a slot with a different subclass than is named by its
:type keyword. This is probably only needed with color, as a substitute for rl-color. Similar for
C-FIELD, when the C-FIELD name is something different from SLOT-NAME-WITHOUT-%.

:pt-accessors is a list of pass-through accessors tied to a backing C struct. These are *not* CLOS
slots, but they will have custom readers and probably writers. Expected format:
(ACCESSOR-NAME TYPE &optional COERCE-TYPE)
COERCE-TYPE will usually be float, when applicable.

:unload is a two-element list: (FN WINDOW-REQUIRED-P). Pass this when an unload function should
be run in the finalizer.

:fn is a function that takes one argument, the object. Will be called at the end."
  (let ((obj (gensym))
        (class-slots (closer-mop:class-direct-slots
                      (find-class class))))
    (labels ((slot-def (slot-name)
               (find slot-name
                     class-slots
                     :test #'(lambda (name def)
                               (eql (closer-mop:slot-definition-name def) name))))
             (slot-arg (slot-name)
               (alexandria:when-let ((def (slot-def slot-name)))
                 (alexandria:symbolicate
                  (format nil "~a"
                          (car (closer-mop:slot-definition-initargs def))))))             
             (slot-type (slot-name)
               (alexandria:when-let ((def (slot-def slot-name)))
                 (closer-mop:slot-definition-type def)))
             (arg-supplied-p (arg)
               (alexandria:symbolicate arg "-SUPPLIED-P")))
      (let ((lisp-slot-args (mapcar #'(lambda (slot) (slot-arg (car slot))) lisp-slots))
            (lisp-slot-types (mapcar #'(lambda (slot) (slot-type (car slot))) lisp-slots))
            (struct-slot-args (mapcar #'(lambda (slot) (slot-arg (car slot))) struct-slots))
            (struct-slot-types (mapcar #'(lambda (slot) (slot-type (car slot))) struct-slots)))
        `(defmethod initialize-instance
             :after ((,obj ,class)
                     &rest initargs
                     &key ,@(remove nil (append (mapcar #'(lambda (arg)
                                                            `(,arg nil ,(arg-supplied-p arg)))
                                                        (remove nil lisp-slot-args))
                                                struct-slot-args
                                                (mapcar #'car pt-accessors)))
                       &allow-other-keys)
           ,@(loop for arg in (append lisp-slot-args struct-slot-args)
                   for type in (append lisp-slot-types struct-slot-types)
                   when arg
                     collect `(check-type ,arg (or ,type null)))
           ,@(expand-check-types pt-accessors t)
           (when (and (slot-exists-p ,obj '%c-ptr)
                      (not (slot-boundp ,obj '%c-ptr)))
              (setf (c-ptr ,obj)
                    (make-instance 'c-ptr :c-ptr (getf initargs :c-ptr))))
           ,@(loop for arg in lisp-slot-args
                   for type in lisp-slot-types
                   for slot in lisp-slots
                   when arg
                     collect (destructuring-bind (name &optional use-writer-p coerce-type) slot
                               (let ((setter `(setf ,(if use-writer-p
                                                         `(,arg ,obj)
                                                         `(slot-value ,obj ',name))
                                                    ,(if coerce-type
                                                         `(coerce ,arg ',coerce-type)
                                                         arg))))
                                 `(when ,(arg-supplied-p arg) ,setter))))
           ,@(loop for arg in struct-slot-args
                   for type in struct-slot-types
                   for slot in struct-slots
                   collect (destructuring-bind (name &optional subclass c-field) slot
                             `(if ,arg
                                  (set-slot ,(alexandria:make-keyword arg) ,obj ,arg)
                                  (setf (slot-value ,obj ',name)
                                        (make-instance ',(or subclass type)
                                                       :finalize nil
                                                       :c-ptr (field-value
                                                               (c-ptr ,obj)
                                                               ',(alexandria:symbolicate
                                                                  (subseq (write-to-string class) 3))
                                                               ',(or c-field
                                                                     (alexandria:symbolicate
                                                                      (subseq
                                                                       (write-to-string name)
                                                                       1)))))))))
           ,@(loop for accessor in pt-accessors
                   collect (destructuring-bind (name type &optional coerce-type) accessor
                             (let ((val (if coerce-type
                                            `(coerce ,name ',coerce-type)
                                            name)))
                               (if (eql type 'boolean)
                                   `(setf (,name ,obj) ,val)
                                   `(when ,name (setf (,name ,obj) ,val))))))
           ,(when unload
              (let ((ptr (gensym)))
                `(when (getf initargs :finalize)
                   (tg:cancel-finalization (slot-value ,obj '%c-ptr))
                   (tg:finalize (slot-value ,obj '%c-ptr)
                                (let ((,ptr (c-ptr ,obj)))
                                  (lambda ()
                                    ,(if (cadr unload)
                                         `(when (is-window-ready-p) (,(car unload) ,ptr))
                                         `(,(car unload) ,ptr))
                                    (cffi:foreign-free ,ptr)))))))
           ,(when fn
              `(funcall ,fn ,obj))
           ,obj)))))

(defmacro child-setter (type &rest slots)
  "Define :before methods on writers for the purpose of running triggers on children that have them.
Use this on slots with :accessor."
  `(progn
     ,@(loop for slot in slots
             collect `(defmethod (setf ,slot) :before (value (obj ,type))
                        (set-linked-children ',slot obj value)))))

(defmacro default-unload (type fn &optional window-required-p)
  "Define an initializer for a C wrapper type which will add a UNLOAD-* function to its finalizer."
  (let ((obj (gensym))
        (ptr (gensym)))
    `(defmethod initialize-instance :after ((,obj ,type) &key)
       (tg:cancel-finalization ,obj)
       (tg:finalize ,obj
                    (let ((,ptr (c-ptr ,obj)))
                      (lambda ()
                        ,(if window-required-p
                             `(when (is-window-ready-p) (,fn ,obj))
                             `(,fn ,obj))
                        (cffi:foreign-free ,ptr)))))))

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
                       `(c-ptr ,accessor))
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
     (,c-fn ,@(expand-c-fun-args args))))

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
unless ALLOCATE-OR-INTO is passed. ALLOCATE-FORM is the form that is evaluated in the latter case.
Each ARG is expected to have the following format:

(ACCESSOR TYPE &optional COERCE-TYPE DEFAULT-VALUE)"
  `(defun ,name (,@(mapcar #'car args) &optional allocate-or-into)
     ,docstring
     ,@(expand-check-types args)
     (check-type allocate-or-into (or boolean ,(cadar args)))
     (let ((retval (cond
                     ((eql allocate-or-into t) ,allocate-form)
                     ((not allocate-or-into) ,(caar args))
                     (t allocate-or-into))))
       (,c-fn (c-ptr retval)
              ,@(expand-c-fun-args args))
       retval)))

(defmacro define-print-object (type methods)
  "Defines PRINT-OBJECT for TYPE. METHODS should contain function names which
will be called with the object as the parameter. The name of the method and
it's result will be printed in the output as `:METHOD (method obj)`. The first
call to PRINT-OBJECT will use PRINT-UNREADABLE-OBJECT, while next-methods will
only print to the same object.

E.g. (define-print-object sometype (x y)) will print something like `#<SOMETYPE :x 10 :y 20>`"
  `(defmethod print-object ((obj ,type) out)
     (let* ((class (find-class ',type))
            (hierarchy (closer-mop:compute-class-precedence-list (find-class (type-of obj))))
            (top (car hierarchy))
            ;; Last class we want to print, i.e. the last class before STANDARD-OBJECT
            ;; Skip STANDARD-OBJECT, SLOT-CLASS and T
            (last (cadddr (reverse hierarchy)))
            (print-methods (lambda () (dolist (method ',methods)
                                        (let ((value (handler-case (funcall (symbol-function method) obj)
                                                       (unbound-slot () "#<unbound-slot>"))))
                                          (format out ":~A ~A " method value))))))
       (if (eq top class)
           (print-unreadable-object (obj out :type t)
             (funcall print-methods)
             (unless (eq last class)
               (call-next-method)))
           (progn
             (funcall print-methods)
             (unless (eq last class)
               (call-next-method)))))))

(defmacro static-draw (name type)
  "Define a draw function for a specific type, enabling static dispatch on DRAW-OBJECT."
  `(defun ,name (obj)
     (declare (optimize (speed 3) (safety 0))
              (type ,type obj))
     (draw-object obj)))
