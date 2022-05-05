(in-package #:claylib)

(defclass game-scene ()
  ((%game-objects :initarg :objects
                  :type hash-table
                  :initform (make-hash-table :test #'equalp)
                  :accessor objects)
   (%game-assets :initarg :assets
                 :type hash-table
                 :initform (make-hash-table :test #'equalp)
                 :accessor assets)))

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
  (dolist (obj (nreverse (alexandria:hash-table-values (objects scene))))
    (draw-object obj)))

(defun draw-scene-except (scene &rest names)
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

(defun make-scene (assets objects)
  (let ((scene (make-instance 'game-scene)))
    (dolist (asset assets)
      (setf (gethash (car asset) (assets scene)) (cadr asset)))
    (dolist (object objects)
      (setf (gethash (car object) (objects scene)) (cadr object)))
    scene))

(defun scene-object (scene object)
  (gethash object (objects scene)))

(defmacro with-scene-objects (objects scene &body body)
  `(symbol-macrolet ,(loop for obj in objects
                           collect (if (listp obj)
                                       `(,(car obj) (gethash ,(cadr obj) (objects ,scene)))
                                       `(,obj (gethash ',obj (objects ,scene)))))
     ,@body))


(defmacro with-scene (scene (&key (free :now)) &body body)
  `(progn
     (load-scene-all ,scene)
     ,@body
     ,(case free
        (:now `(progn
                 (unload-scene-all ,scene)
                 (collect-garbage)))
        (:later `(unload-scene-all-later ,scene))
        (:never nil)
        (t (error ":FREE must be :NOW, :LATER, or :NEVER")))))
