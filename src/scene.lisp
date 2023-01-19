(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass game-scene ()
    ((%parameters :initarg :params
                  :type hash-table
                  :initform (make-hash-table :test #'equalp)
                  :accessor params)
     (%game-objects :initarg :objects
                    :type hash-table
                    :initform (make-hash-table :test #'equalp)
                    :accessor objects)
     (%game-assets :initarg :assets
                   :type hash-table
                   :initform (make-hash-table :test #'equalp)
                   :accessor assets)
     (%gc :initarg :gc
          :type boolean
          :accessor gc)
     (%active
      :initform nil
      :type boolean
      :accessor active))))

(define-print-object game-scene
    (params objects assets gc active))

(defun load-scene (scene &rest names)
  (dolist (asset names)
    (load-asset (gethash asset (assets scene)))))

(defun load-scene-all (scene)
  (dolist (asset (alexandria:hash-table-values (assets scene)))
    (load-asset asset)))

(defun load-scene-except (scene &rest names)
  (dolist (asset (remove-if (lambda (kv)
                              (member (car kv) names))
                            (alexandria:hash-table-alist (assets scene))))
    (load-asset (cdr asset))))

(defun draw-scene (scene &rest names)
  "Draw the objects in SCENE referred to by the symbols in NAMES."
  (dolist (obj names)
    (draw-object (scene-object scene obj))))

(defun draw-objects (&rest objects)
  "Draw the given OBJECTS without having to specify a scene.

This is handy when the objects are in scope already, for example via WITH-SCENE-OBJECTS."
  (dolist (obj objects)
    (draw-object obj)))

(defun draw-scene-all (scene)
  "Draw all the objects in the given SCENE."
  (dolist (obj (nreverse (alexandria:hash-table-values (objects scene))))
    (draw-object obj)))

(defun draw-scene-except (scene &rest names)
  "Draw all the objects in the given SCENE except those specified as one of NAMES."
  (dolist (obj (remove-if (lambda (kv)
                            (member (car kv) names))
                          (nreverse (alexandria:hash-table-alist (objects scene)))))
    (draw-object (cdr obj))))

(defun draw-scene-regex (scene regex)
  "Draw the objects in the SCENE whose name matches the given Perl REGEX."
  (loop for kv in (nreverse (alexandria:hash-table-alist (objects scene)))
        when (cl-ppcre:scan regex (symbol-name (car kv)))
          do (draw-object (cdr kv))))

(defmacro make-scene (assets objects &key (gc t) (defer-init t))
  "Make a GAME-SCENE.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually via WITH-SCENES or
SET-UP-SCENE directly). This is useful when your scene contains objects like TEXTURES which require
an OpenGL context before being loaded into the GPU.

GC expresses a preference for whether garbage collection should be run when the scene closes.
This can be overridden in WITH-SCENES.

ASSETS and OBJECTS are lists of items of the form (BINDING VALUE). In a nutshell: assets are
things that get loaded, and objects are things that get drawn. Values can reference previous
bindings but will be initialized in the order they are declared."
  (let* ((scene (gensym))
         (items (append assets objects))
         (syms (when defer-init
                 (alexandria:make-gensym-list (length items)))))
    `(let ((,scene (make-instance 'game-scene :gc ,gc))
           ,@syms)
       (declare (ignorable ,@syms))
       ,(if defer-init
            `(symbol-macrolet (,@(loop for sym in syms
                                       for item in items
                                       collect `(,(car item) (eager-future2:touch ,sym))))
               (let* (,@(loop for sym in syms
                              for item in items
                              collect `(,sym (eager-future2:pcall
                                              (lambda () ,(cadr item))
                                              :lazy))))
                 ,@(loop for sym in syms
                         for asset in assets
                         collect `(setf (gethash ',(car asset) (assets ,scene))
                                        ,sym))
                 ,@(loop for sym in (nthcdr (length assets) syms)
                         for obj in objects
                         collect `(setf (gethash ',(car obj) (objects ,scene))
                                        ,sym))))
            `(symbol-macrolet (,@items)
               ,@(loop for (binding val) in assets
                       collect `(setf (gethash ',binding (assets ,scene)) ,binding))
               ,@(loop for (binding val) in objects
                       collect `(setf (gethash ',binding (objects ,scene)) ,binding))))
       ,scene)))

(defmacro make-scene-pro ((&rest components) &key (gc t) (defer-init t))
  "Make a GAME-SCENE.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually via WITH-SCENES or
SET-UP-SCENE directly). This is useful when your scene contains objects like TEXTURES which require
an OpenGL context before being loaded into the GPU.

GC expresses a preference for whether garbage collection should be run when the scene closes.
This can be overridden in WITH-SCENES.

Each COMPONENT is a list whose head is either :PARAMS, :ASSETS, or :OBJECTS. In a nutshell: assets
are things that get loaded, objects are things that get drawn, and parameters are generic bindings
which are neither loaded nor drawn. Items after the head look like (BINDING VALUE). Values can
reference previous bindings but will be initialized in the order they are declared."
  (let* ((scene (gensym))
         (items (reduce #'append
                        (mapcar #'cdr components)))
         (syms (when defer-init
                 (alexandria:make-gensym-list (length items)))))
    (flet ((group-case (group)
             (ecase (car group)
               (:params `(params ,scene))
               (:assets `(assets ,scene))
               (:objects `(objects ,scene)))))
      `(let ((,scene (make-instance 'game-scene :gc ,gc))
             ,@syms)
         (declare (ignorable ,@syms))
         ,(if defer-init
              `(symbol-macrolet (,@(loop for sym in syms
                                         for item in items
                                         collect `(,(car item) (eager-future2:touch ,sym))))
                 (let* (,@(loop for sym in syms
                                for item in items
                                collect `(,sym (eager-future2:pcall
                                                (lambda () ,(cadr item))
                                                :lazy))))
                   ,@(loop for group in components
                           with i = -1
                           append (mapcar #'(lambda (item)
                                              (incf i)
                                              `(setf (gethash ',(car item) ,(group-case group))
                                                     ,(elt syms i)))
                                          (cdr group)))))
              `(symbol-macrolet (,@items)
                 ,@(loop for group in components
                         append (mapcar #'(lambda (item)
                                            `(setf (gethash ',(car item) ,(group-case group))
                                                   ,(car item)))
                                        (cdr group)))))
         ,scene))))

(defun scene-object (scene object)
  (gethash object (objects scene)))

(defun scene-asset (scene asset)
  (gethash asset (assets scene)))

(defun scene-param (scene param)
  (gethash param (params scene)))

(defmacro with-scene-assets (assets scene &body body)
  "Define local symbol macros to conveniently access specific scene assets. ASSETS is a list;
each element is either the name of an asset or a symbol/value pair, a la WITH-SLOTS."
  `(symbol-macrolet ,(loop for ass in assets
                           collect (if (listp ass)
                                       `(,(car ass) (gethash ,(cadr ass) (assets ,scene)))
                                       `(,ass (gethash ',ass (assets ,scene)))))
     ,@body))

(defmacro with-scene-objects (objects scene &body body)
  "Define local symbol macros to conveniently access specific scene objects. OBJECTS is a list;
each element is either the name of an object or a symbol/value pair, a la WITH-SLOTS."
  `(symbol-macrolet ,(loop for obj in objects
                           collect (if (listp obj)
                                       `(,(car obj) (gethash ,(cadr obj) (objects ,scene)))
                                       `(,obj (gethash ',obj (objects ,scene)))))
     ,@body))

(defmacro with-scene-params (params scene &body body)
  "Define local symbol macros to conveniently access specific scene parameters. PARAMS is a list;
each element is either the name of a parameter or a symbol/value pair, a la WITH-SLOTS."
  `(symbol-macrolet ,(loop for param in params
                           collect (if (listp param)
                                       `(,(car param) (gethash ,(cadr param) (params ,scene)))
                                       `(,param (gethash ',param (params ,scene)))))
     ,@body))

(defun dynamic-bindings (scene what body)
  (let ((bindings (concatenate 'list
                               (when (member :assets what)
                                 (loop for k in (alexandria:hash-table-keys (assets scene))
                                       collect `(,k (gethash ',k (assets ,scene)))))
                               (when (member :objects what)
                                 (loop for k in (alexandria:hash-table-keys (objects scene))
                                       collect `(,k (gethash ',k (objects ,scene)))))
                               (when (member :params what)
                                 (loop for k in (alexandria:hash-table-keys (params scene))
                                       collect `(,k (gethash ',k (params ,scene))))))))
    (funcall (compile nil `(lambda ()
                             (symbol-macrolet (,@bindings)
                               ,@body))))))

(defmacro with-scene-bindings ((&rest what) scene &body body)
  "Evaluate BODY within the context of a scene, using symbol macros bound to the same names
used as keys. WHAT is any/all of: (:ASSETS :OBJECTS :PARAMS) -- the default is all three.

This has two important limitations!

1) You lose access to the lexical context outside of WITH-SCENE-BINDINGS.

2) Any new objects added to the scene within this context will not automatically get bindings.

If the first limitation is undesirable, you may want to try WITH-SCENE-ASSETS/OBJECTS/PARAMS
instead. If the second limitation is undesirable, you're welcome to add that feature yourself."
  (let ((what (or what '(:assets :objects :params))))
    `(dynamic-bindings ,scene ',what ',body)))

(defmacro with-scenes (scenes (&key (gc nil gc-supplied-p)) &body body)
  "Execute BODY after loading & initializing SCENES, tearing them down afterwards.
Pass :GC (T or NIL) to force/unforce garbage collection, overriding what the scenes request.

Note: additional scenes can be loaded/GC'd at any point using {SET-UP,TEAR-DOWN}-SCENE."
  (unless (listp scenes) (setf scenes `(list ,scenes)))
  (let ((sym (gensym)))
    `(progn
       (mapcar #'set-up-scene ,scenes)
       (mapcar #'(lambda (,sym) (setf (active ,sym) t)) ,scenes)
       ,@body
       (mapcar #'(lambda (,sym) (setf (active ,sym) nil)) ,scenes)
       ,(cond
          ((and gc-supplied-p gc) `(tg:gc :full t))
          ((not gc-supplied-p) `(mapcar #'tear-down-scene ,scenes))
          (t nil)))))

(defmethod set-up-scene ((scene game-scene))
  (flet ((yield-things (ht)
           (maphash (lambda (binding val)
                      "Yield the futures in the hash table in place."
                      (when (typep val 'eager-future2:future)
                        (setf (gethash binding ht) (eager-future2:yield val))))
                    ht)))
    (yield-things (assets scene))
    (load-scene-all scene)
    (yield-things (params scene))
    (yield-things (objects scene))))

(defmethod set-up-scene ((scene null)) ())

(defmethod tear-down-scene ((scene game-scene))
  (when (gc scene) (tg:gc :full t)))

(defmethod tear-down-scene ((scene null)) ())

(defun add-to-scene (scene what key value &optional (defer (not (active scene))))
  "Add another element to a scene. WHAT is one of: :ASSET, :OBJECT, or :PARAM.
KEY and VALUE are a hash table key and form to be evaluated, respectively.

If DEFER is NIL, the element will be evaluated immediately. Otherwise, it gets
added as a future as if defined in MAKE-SCENE. You must set DEFER correctly or
your scene will break. When in doubt, trust the default."
  (let ((ht (ecase what
              (:asset (assets scene))
              (:object (objects scene))
              (:param (params scene)))))
    (if defer
        (setf (gethash key ht) (eager-future2:pcall
                                (lambda () value)
                                :lazy))
        (progn
          (setf (gethash key ht) value)
          (when (eql what :asset)
            (load-asset value))))))

(defun remove-from-scene (scene what key)
  "Remove an element from a scene. WHAT is one of: :ASSET, :OBJECT, or :PARAM."
  (remhash key (ecase what
                 (:asset (assets scene))
                 (:object (objects scene))
                 (:param (params scene)))))
