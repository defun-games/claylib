;;;; claylib.lisp

(in-package #:claylib)

(defparameter +claylib-directory+ (asdf:system-source-directory :claylib))

(defvar *garbage* ())

(defun free-later (thing)
  (pushnew thing *garbage*))

(defun collect-garbage ()
  (mapcar #'free *garbage*)
  (setf *garbage* ()))

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

(defun clear-background (&key (color *claylib-background*))
  "Set background to COLOR or *CLAYLIB-BACKGROUND* by default."
  (claylib/ll:clear-background (c-struct color)))

(defmacro with-texture-mode ((render-texture &key (clear *claylib-background*)) &body body)
  "Execute BODY while drawing to the given RENDER-TEXTURE. CLEAR is the RL-COLOR to set the initial
background of the render texture, or NIL to skip clearing."
  `(progn
     (begin-texture-mode (c-struct ,render-texture))
     (when ,clear
       (clear-background :color ,clear))
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
