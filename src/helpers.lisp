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

'To read the ROT value of a CAMERA-2D, read the ROTATION value of the CAMERA2D struct backing it.'
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

'To set the ROT value of a CAMERA-2D, set the ROTATION value of the CAMERA2D struct backing it. 
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

(defmacro definitializer (type &rest slots)
  (let ((obj (gensym))
        (ptr (gensym)))
    `(defmethod initialize-instance :after ((,obj ,type)
                                            &key ,@(mapcar #'(lambda (slot)
                                                               (if (= (length slot) 4)
                                                                   `(,(car slot) ,(fourth slot))
                                                                   (car slot)))
                                                           slots))
       ,@(loop for slot in slots
               collect (destructuring-bind (accessor type &optional coerce-type default-value) slot
                         (declare (ignore coerce-type default-value))
                         `(check-type ,accessor (or ,type null))))
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
       (when (and (member-if #'(lambda (slot)
                                 (eql (closer-mop:slot-definition-name slot) '%c-struct))
                             (closer-mop:class-direct-slots (find-class ',type)))
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
         (when (or (and ,window-required-p (is-window-ready-p))
                   ,(and fn (not window-required-p)))
           (,fn ,obj))
         (autowrap:free ,obj)))))

(defmacro default-slot-value (class slot-name value)
  "Define a SLOT-UNBOUND method as a lazy fallback default slot value."
  (let ((obj (gensym))
        (slot (gensym)))
    `(defmethod slot-unbound (,class ,obj (,slot (eql ',slot-name)))
       (setf (slot-value ,obj ,slot) ,value))))
