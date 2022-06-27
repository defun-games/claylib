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
   (%free :initarg :free
          :type keyword
          :accessor free)))

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
  (dolist (thing names)
    (free (gethash thing (assets scene)))
    (free (gethash thing (objects scene)))))

(defun unload-scene-all (scene)
  (dolist (asset (alexandria:hash-table-values (assets scene)))
    (free asset))
  (dolist (obj (alexandria:hash-table-values (objects scene)))
    (free obj)))

(defun unload-scene-except (scene &rest names)
  (dolist (asset (remove-if (lambda (kv)
                              (member (car kv) names))
                            (alexandria:hash-table-alist (assets scene))))
    (free (cdr asset)))
  (dolist (obj (remove-if (lambda (kv)
                            (member (car kv) names))
                          (alexandria:hash-table-alist (objects scene))))
    (free (cdr obj))))

(defun unload-scene-later (scene &rest names)
  (dolist (thing names)
    (free-later (gethash thing (assets scene)))
    (free-later (gethash thing (objects scene)))))

(defun unload-scene-all-later (scene)
  (dolist (asset (alexandria:hash-table-values (assets scene)))
    (free-later asset))
  (dolist (obj (alexandria:hash-table-values (objects scene)))
    (free-later obj)))

(defun unload-scene-except-later (scene &rest names)
  (dolist (asset (remove-if (lambda (kv)
                              (member (car kv) names))
                            (alexandria:hash-table-alist (assets scene))))
    (free-later (cdr asset)))
  (dolist (obj (remove-if (lambda (kv)
                            (member (car kv) names))
                          (alexandria:hash-table-alist (objects scene))))
    (free-later (cdr obj))))

(defun draw-scene (scene &rest names)
  (dolist (obj names)
    (draw-object (scene-object scene obj))))

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
  "Make a GAME-SCENE.

DEFER-INIT will defer initialization of the scene's OBJECTS until later (usually in DO-GAME-LOOP).
This is useful for objects like TEXTURES which require an OpenGL context to be loaded into the GPU."
  (let ((scene (gensym))
        (objects (if defer-init
                     (loop for (binding val) in objects
                           collect `(,binding (eager-future2:pcall (lambda () ,val) :lazy)))
                     objects)))
    `(let ((,scene (make-instance 'game-scene :free ,free)))
       (let* (,@assets ,@objects)
         (declare (ignorable ,@(mapcar #'car (append assets objects))))
         (progn
           ,@(loop for (binding val) in assets
                   collect `(setf (gethash ',binding (assets ,scene)) ,val))
           ,@(loop for (binding val) in objects
                   collect `(setf (gethash ',binding (objects ,scene)) ,val))))
       ,scene)))

(defun scene-object (scene object)
  (gethash object (objects scene)))

(defmacro with-scene-objects (objects scene &body body)
  `(symbol-macrolet ,(loop for obj in objects
                           collect (if (listp obj)
                                       `(,(car obj) (gethash ,(cadr obj) (objects ,scene)))
                                       `(,obj (gethash ',obj (objects ,scene)))))
     ,@body))


(defmacro with-scenes (scenes &body body)
  "Execute BODY after loading & initializing SCENES, tearing them down afterwards.

Note: additional scenes can be loaded/freed within the loop using {SET-UP,TEAR-DOWN}-SCENE."
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
