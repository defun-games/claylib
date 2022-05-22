;;;; claylib.lisp

(in-package #:claylib)

(defparameter +claylib-directory+ (asdf:system-source-directory :claylib))

(defvar *garbage* ())

(defun free-later (thing)
  (pushnew thing *garbage*))

(defun collect-garbage ()
  (mapcar #'free *garbage*)
  (setf *garbage* ()))






















(defclass game-asset ()
  ((%path :initarg :path
          :type pathname
          :accessor path)
   (%ptr
    ; :type TODO -- cross-implementation SB-SYS:SYSTEM-AREA-POINTER?
    :initform nil
    :accessor ptr)))





#|
(defclass model (game-asset) ())

(defmethod free ((asset model))
  (claylib/ll:unload-model (c-ref (ptr asset) claylib/ll:model))
  (setf (ptr asset) nil))

(defmethod initialize-instance :after ((model model) &key load-now)
  (when load-now (load-model model)))
|#









(defclass shader (game-asset)
  ((%path :initform nil)
   (%vspath :initarg :vspath
            :type pathname
            :accessor vspath)
   (%fspath :initarg :fspath
            :type pathname
            :accessor fspath)))

(defmethod load-asset ((asset shader) &key force-reload)
  (if (or force-reload (null (ptr asset)))
      (c-let ((c claylib/ll:shader))
        (claylib/ll:load-shader c (vspath asset) (fspath asset))
        (setf (ptr asset) (autowrap:ptr c)))
      (ptr asset)))

(defmethod free ((asset shader))
  (claylib/ll:unload-shader (c-ref (ptr asset) claylib/ll:shader))
  (setf (ptr asset) nil))

(defmethod initialize-instance :after ((shader shader) &key load-now)
  (when load-now (load-shader shader)))






(defclass model-animation (game-asset)
  ((%num
    :type integer
    :accessor num)))

(defmethod load-asset ((asset model-animation) &key force-reload)
  (if (or force-reload (null (ptr asset)))
      (c-let ((c claylib/ll:model-animation)
              (i :int))
        (setf c (claylib/ll:load-model-animations (path asset) (i &))
              (ptr asset) (autowrap:ptr c)
              (num asset) i))
      (ptr asset)))

(defmethod free ((asset model-animation))
  (claylib/ll:unload-model-animations (c-ref (ptr asset) claylib/ll:model-animation) (num asset))
  (setf (ptr asset) nil))

(defmethod initialize-instance :after ((anim model-animation) &key load-now)
  (when load-now (load-model-animation anim)))






(defmacro with-2d-mode (camera &body body)
  `(progn
     (begin-mode2d (c-struct ,camera))
     ,@body
     (end-mode2d)))

(defmacro with-3d-mode (camera &body body)
  `(progn
     (begin-mode3d (c-struct ,camera))
     ,@body
     (end-mode3d)))

(defmacro with-texture-mode (texture &body body)
  `(progn
     (begin-texture-mode (c-struct ,texture))
     (clear-background (c-struct *claylib-background*))
     ,@body
     (end-texture-mode)))

(defmacro do-game-loop ((&key
                           (livesupport nil)
                           (vars ())
                           (end ())
                           (result ()))
                        &body body)
  `(do ,vars ((or (window-should-close-p) ,end) ,result)
     ,@(when livesupport `((declare (notinline))))
     ,(if livesupport
          `(livesupport:continuable
             ,@body
             (livesupport:update-repl-link))
          `(progn ,@body))))

(defmacro with-window ((&key
                          (width *screen-width*)
                          (height *screen-height*)
                          (title "")
                          (fps *target-fps*)
                          (flags ())
                          (min-size ()))
                       &body body)
  `(progn
     (claylib/ll:init-window ,width ,height ,title)
     (claylib/ll:set-target-fps ,fps)
     (setf +default-font+ (load-font-default))
     ,(when flags
        `(claylib/ll:set-config-flags (reduce #'+ ,flags)))
     ,(when min-size
        `(claylib/ll:set-window-min-size ,(car min-size) ,(cadr min-size)))
     ,@body
     (free +default-font+)
     (collect-garbage)
     (when (is-window-ready-p)
       (close-window))))
