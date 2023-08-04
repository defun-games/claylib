;;;; claylib.lisp

(in-package #:claylib)

(defparameter +claylib-directory+ (asdf:system-source-directory :claylib))

(defparameter +keyword-flag+ (list :hidden            +flag-window-hidden+
                                   :topmost           +flag-window-topmost+
                                   :highdpi           +flag-window-highdpi+
                                   :minimized         +flag-window-minimized+
                                   :maximized         +flag-window-maximized+
                                   :unfocused         +flag-window-unfocused+
                                   :resizable         +flag-window-resizable+
                                   :always-run        +flag-window-always-run+
                                   :undecorated       +flag-window-undecorated+
                                   :transparent       +flag-window-transparent+
                                   :mouse-passthrough +flag-window-mouse-passthrough+
                                   :vsync-hint        +flag-vsync-hint+
                                   :msaa-4x-hint      +flag-msaa-4x-hint+
                                   :fullscreen-mode   +flag-fullscreen-mode+
                                   :interlaced-hint   +flag-interlaced-hint+))

(defmacro with-2d-mode (camera &body body)
  `(progn
     (begin-mode-2d (c-ptr ,camera))
     ,@body
     (end-mode-2d)))

(defmacro with-3d-mode (camera &body body)
  `(progn
     (begin-mode-3d (c-ptr ,camera))
     ,@body
     (end-mode-3d)))

(defun clear-background (&key (color *claylib-background*))
  "Set background to COLOR or *CLAYLIB-BACKGROUND* by default."
  (claylib/ll:clear-background (c-ptr color)))

(defmacro with-drawing ((&key (bgcolor *claylib-background*)) &body body)
  "Convenience macro around double-buffer drawing."
  `(progn
     (begin-drawing)
     (clear-background :color ,bgcolor)
     ,@body
     (end-drawing)))

(defmacro with-texture-mode ((render-texture &key (clear *claylib-background*)) &body body)
  "Execute BODY while drawing to the given RENDER-TEXTURE. CLEAR is the RL-COLOR to set the initial
background of the render texture, or NIL to skip clearing."
  `(progn
     (begin-texture-mode (c-ptr ,render-texture))
     ,(when clear
       `(clear-background :color ,clear))
     ,@body
     (end-texture-mode)))

(defmacro do-game-loop ((&key
                           (livesupport nil)
                           (vars ())
                           (end ())
                           (result ()))
                        &body body)
  "Execute a game loop.

When given, this will enable LIVESUPPORT during execution of the loop, expose the bindings in VARS
to the loop BODY, stop the loop when END is non-nil, and return RESULT."
  `(do ,vars ((or (window-should-close-p) ,end)
              ,result)
     ,@(when livesupport `((declare (notinline))))
     ,(if livesupport
          `(livesupport:continuable
             ,@body
             (livesupport:update-repl-link))
          `(progn ,@body))))

(defun flag->code (flag)
    (let ((code (getf +keyword-flag+
                      flag)))
        (if code
            code
            (error "The ~A flag is unknown." flag))))

(defun flags->codes (flags)
    (loop for flag in flags
          collect (flag->code flag)

(defmacro with-window ((&key
                          (width *screen-width*)
                          (height *screen-height*)
                          (title "")
                          (fps *target-fps*)
                          (flags ())
                          (min-size ())
                          (gui-style nil)
                          exit-key)
                       &body body)
  `(progn
     ,(when flags
        `(claylib/ll:set-config-flags (reduce #'+ `,(flags->codes ,flags))))
     (claylib/ll:init-window ,width ,height ,title)
     (unless (is-audio-device-ready-p) (claylib/ll:init-audio-device))
     (claylib/ll:set-target-fps ,fps)
     (setf +default-font+ (load-font-default))
     ,(if (not gui-style)
         `(gui-load-style-default)
         `(gui-load-style
           (asdf:system-relative-pathname :claylib
                                          ,(format nil "gui-styles/~A/~:*~A.rgs" gui-style))))
     ,(when min-size
        `(claylib/ll:set-window-min-size ,(car min-size) ,(cadr min-size)))
     ,(when exit-key
        `(claylib/ll:set-exit-key ,exit-key))
     ,@body
     (when (is-audio-device-ready-p)
       (claylib/ll:close-audio-device))
     (when (is-window-ready-p)
       (close-window))))

(defmethod draw-object ((obj list))
  (mapc #'draw-object obj))

(defmacro with-audio-device (&body body)
  "Initialize audio device & context for use during the execution of BODY, closing them afterwards.

Note: this must occur before loading music streams. e.g. before setting up a Claylib SCENE which
contains a MUSIC-ASSET (via WITH-SCENES or SET-UP-SCENE)."
  `(unwind-protect (progn (claylib/ll:init-audio-device)
                          ,@body)
     (claylib/ll:close-audio-device)))
