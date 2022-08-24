(in-package #:claylib)

(defclass game-scene ()
  ((%game-objects :initarg :objects
                  :type hash-table
                  :initform (make-hash-table :test #'equalp)
                  :accessor objects)
   (%game-assets :initarg :assets
                 :type hash-table
                 :initform (make-hash-table :test #'equalp)
                 :accessor assets)
   (%parameters :initarg :parameters
                :type hash-table
                :initform (make-hash-table :test #'equalp)
                :accessor parameters
                :documentation "Things which are associated with a scene, are neither drawn nor
loaded, but still may need to be freed according to the scene's freeing logic.")
   (%free :initarg :free
          :type keyword
          :accessor free
          :documentation "Determines when to free %GAME-ASSETS, %GAME-OBJECTS, & %PARAMETERS.")))

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

(defun unload-scene (scene &rest names)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing names)
             (free (gethash thing (funcall accessor scene))))))

(defun unload-scene-all (scene)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing (alexandria:hash-table-values (funcall accessor scene)))
             (free thing))))

(defun unload-scene-except (scene &rest names)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing (remove-if (lambda (kv)
                                       (member (car kv) names))
                                     (alexandria:hash-table-alist (funcall accessor scene))))
             (free (cdr thing)))))

(defun unload-scene-later (scene &rest names)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing names)
             (free-later (gethash thing (funcall accessor scene))))))

(defun unload-scene-all-later (scene)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing (alexandria:hash-table-values (funcall accessor scene)))
             (free-later thing))))

(defun unload-scene-except-later (scene &rest names)
  (loop for accessor in (list #'assets #'objects #'parameters)
        do (dolist (thing (remove-if (lambda (kv)
                                       (member (car kv) names))
                                     (alexandria:hash-table-alist (funcall accessor scene))))
             (free-later (cdr thing)))))

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

(defmacro defscene (name assets objects)
  ;; TODO: free old scene when reloading
  `(progn
;     (unintern ',name)
     (defvar ,name
       ,(let ((sym (gensym)))
          `(let ((,sym (make-instance 'game-scene)))
             (dolist (asset ,assets)
               (setf (gethash (car asset) (assets ,sym)) (cadr asset)))
             (dolist (object ,objects)
               (setf (gethash (car object) (objects ,sym)) (cadr object)))
             ,sym)))))

(defmacro make-scene (assets objects &key (free :now) (defer-init t))
  "Make a GAME-SCENE from the given ASSETS and OBJECTS.

FREE (one of :now, :later, or :never) determines when to free the objects and assets in a scene. See
TEAR-DOWN-SCENE for details.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually via WITH-SCENES or
SET-UP-SCENE directly). This is useful when your scene contains objects like TEXTURES which require
an OpenGL context before being loaded into the GPU."
  (let ((scene (gensym))
        (objects (if defer-init
                     (loop for (binding val) in objects
                           collect `(,binding (eager-future2:pcall (lambda () ,val) :lazy)))
                     objects)))
    `(let ((,scene (make-instance 'game-scene :free ,free)))
       (let* (,@assets ,@objects)
         (declare (ignorable ,@(mapcar #'car (append assets objects))))
         ,@(loop for (binding val) in assets
                 collect `(setf (gethash ',binding (assets ,scene)) ,val))
         ,@(loop for (binding val) in objects
                 collect `(setf (gethash ',binding (objects ,scene)) ,val)))
       ,scene)))

(defmacro make-scene-pro (groups &key (free :now) (defer-init t))
  "Make a GAME-SCENE from the given GROUPS of assets, objects, or parameters.

GROUPS is a plist whose indicators are the group type (one of :ASSETS, :OBJECTS, or :PARAMETERS) and
whose values are lists of name-form pairs.

FREE and DEFER-INIT act the same way as in MAKE-SCENE.

For example, here we make a scene with an asset group and an object group. Neither the objects nor
assets will be freed automatically. The objects also have their initialization deferred by default:

(make-scene-pro (:assets
                 ((texture-asset (make-texture-asset \"path/to/texture.png\"))
                  (font-asset    (make-font-asset \"path/to/font.ttf\")))
                 :objects
                 ((ball (make-circle 10 10 5 +green+))))
                :free :never)"
  (let ((scene (gensym))
        (objects (if defer-init
                     (loop for (binding val) in (getf groups :objects)
                           collect `(,binding (eager-future2:pcall (lambda () ,val) :lazy))
                             into deferred-objects
                           finally (setf (getf groups :objects) deferred-objects)
                                   (return deferred-objects))
                     (getf groups :objects)))
        (assets (getf groups :assets))
        (parameters (getf groups :parameters))
        (group-contents (reduce #'append (loop for (_ group) on groups by #'cddr collect group))))
    `(let ((,scene (make-instance 'game-scene :free ,free)))
       (let* ,group-contents
         (declare (ignorable ,@(mapcar #'car group-contents)))
         ,@(loop for (binding val) in parameters
                 collect `(setf (gethash ',binding (parameters ,scene)) ,val))
         ,@(loop for (binding val) in assets
                 collect `(setf (gethash ',binding (assets ,scene)) ,val))
         ,@(loop for (binding val) in objects
                 collect `(setf (gethash ',binding (objects ,scene)) ,val)))
       ,scene)))

(defun scene-object (scene object)
  (gethash object (objects scene)))

(defmacro with-scene-objects (objects scene &body body)
  `(symbol-macrolet ,(loop for obj in objects
                           collect (if (listp obj)
                                       `(,(car obj) (gethash ,(cadr obj) (objects ,scene)))
                                       `(,obj (gethash ',obj (objects ,scene)))))
     ,@body))

(defun scene-param (scene parameter)
  (gethash parameter (parameters scene)))

(defmacro with-scene-params (parameters scene &body body)
  `(symbol-macrolet ,(loop for param in parameters
                           collect (if (listp param)
                                       `(,(car param) (gethash ,(cadr param) (parameters ,scene)))
                                       `(,param (scene-param ,scene ',param))))
     ,@body))

(defmacro with-scenes (scenes &body body)
  "Execute BODY after loading & initializing SCENES, tearing them down afterwards.

Note: additional scenes can be loaded/freed at any point using {SET-UP,TEAR-DOWN}-SCENE."
  (unless (listp scenes) (setf scenes `(list ,scenes)))
  `(progn
     (mapcar #'set-up-scene ,scenes)
     ,@body
     (mapcar #'tear-down-scene ,scenes)))

(defmethod set-up-scene ((scene game-scene))
  (load-scene-all scene)
  (maphash (lambda (binding val)
             "Yield the futures in the objects hash table in place."
             (when (typep val 'eager-future2:future)
               (setf (gethash binding (objects scene)) (eager-future2:yield val))))
           (objects scene)))

(defmethod set-up-scene ((scene null)) ())

(defmethod tear-down-scene ((scene game-scene))
  (case (free scene)
    (:now (progn
            (unload-scene-all scene)
            (collect-garbage)))
    (:later (unload-scene-all-later scene))
    (:never nil)
    (t (error "%FREE must be :NOW, :LATER, or :NEVER"))))

(defmethod tear-down-scene ((scene null)) ())
