(in-package #:claylib/ll)

(defvar *claylib-background* +raywhite+)
(defvar *screen-width* 800)
(defvar *screen-height* 450)
(defvar *target-fps* 60)

(defmacro lisp-bool (name &rest args)
  (let ((c-fun (intern (remove #\' (format nil "~:@a" name)))) 
        (lisp-fun (intern (remove #\' (format nil "~:@a-P" name)))))
    `(defun ,lisp-fun (,@args)
       (declare (inline))
       (= (,c-fun ,@args) 1))))

(lisp-bool window-should-close)
(lisp-bool is-window-ready)
(lisp-bool is-window-fullscreen)
(lisp-bool is-window-hidden)
(lisp-bool is-window-minimized)
(lisp-bool is-window-maximized)
(lisp-bool is-window-focused)
(lisp-bool is-window-resized)
(lisp-bool is-window-state flag)

(lisp-bool is-cursor-hidden)
(lisp-bool is-cursor-on-screen)

(lisp-bool file-exists file-name)
(lisp-bool directory-exists dir-path)
(lisp-bool is-file-extension file-name ext)
(lisp-bool is-file-dropped)

(lisp-bool is-key-pressed key)
(lisp-bool is-key-down key)
(lisp-bool is-key-released key)
(lisp-bool is-key-up key)

(lisp-bool is-gamepad-available gamepad)
(lisp-bool is-gamepad-button-pressed gamepad button)
(lisp-bool is-gamepad-button-down gamepad button)
(lisp-bool is-gamepad-button-released gamepad button)
(lisp-bool is-gamepad-button-up gamepad button)

(lisp-bool is-mouse-button-pressed button)
(lisp-bool is-mouse-button-down button)
(lisp-bool is-mouse-button-released button)
(lisp-bool is-mouse-button-up button)

(lisp-bool is-gesture-detected gesture)

(lisp-bool text-is-equal text1 text2)

(lisp-bool is-model-animation-valid model anim)

(lisp-bool is-audio-device-ready)

(lisp-bool is-sound-playing sound)

(lisp-bool is-music-stream-playing music)

(lisp-bool is-audio-stream-processed stream)
(lisp-bool is-audio-stream-playing stream)

(lisp-bool gui-is-locked)

(declaim (inline run-window-p))
(defun run-window-p ()
  "Returns T if the raylib window status is good and should therefore stay open."
  (not (window-should-close-p)))


(defmacro with-window ((&key
                          (width *screen-width*)
                          (height *screen-height*)
                          (title "")
                          (fps *target-fps*)
                          (flags ())
                          (min-size ()))
                       &body body)
  "Initializes a new game window, runs your code, and closes when finished."
  `(progn
     (init-window ,width ,height ,title)
     (set-target-fps ,fps)
     ,(when flags
       `(set-config-flags (reduce #'+ ,flags)))
     ,(when min-size
       `(set-window-min-size ,(car min-size) ,(cadr min-size)))
     ,@body
     (close-window)))


(defmacro loop-drawing (&body body)
  "Provides a Raylib loop structure which prepares for drawing
  and loops until the user closes the graphics window."
  `(loop while (run-window-p) do
     (begin-drawing)
     (clear-background *claylib-background*)
     ,@body
     (end-drawing)))


(defmacro do-drawing (vars ends &body body)
  "Provides a Raylib-ready do construct which prepares for drawing."
  `(do ,vars ,ends
     (begin-drawing)
     (clear-background *claylib-background*)
     ,@body
     (end-drawing)))


(defmacro with-drawing ((&key (bgcolor *claylib-background*)) &body body)
  "Provides a simple Raylib-ready construct for drawing."
  `(progn
     (begin-drawing)
     (clear-background ,bgcolor)
     ,@body
     (end-drawing)))


(defmacro with-mode2d (cam &body body)
  "Provides a simple Raylib-ready construct for 2D camera mode."
  `(progn
     (begin-mode2d ,cam)
     ,@body
     (end-mode2d)))

(defmacro with-mode3d (cam &body body)
  "Provides a simple Raylib-ready construct for 3D camera mode."
  `(progn
     (begin-mode3d ,cam)
     ,@body
     (end-mode3d)))

(defmacro with-texture-mode ((texture &key (bgcolor *claylib-background*)) &body body)
  "Provides a simple Raylib-ready construct for texture mode."
  `(progn
     (begin-texture-mode ,texture)
     (clear-background ,bgcolor)
     ,@body
     (end-texture-mode)))

(defmacro with-scissor-mode (x y width height &body body)
  "Provides a simple Raylib-ready construct for scissor mode."
  ;; Raylib's scissor mode only supports ints. We could convert but I think it's best
  ;; to leave it up to the user how to do that.
  `(progn
     (begin-scissor-mode ,x ,y ,width ,height)
     ,@body
     (end-scissor-mode)))

#|
(defun set-vector2 (vec x y)
  "Set the coordinate values of a 2D vector."
  (setf (vector2.x vec) (coerce x 'float)
        (vector2.y vec) (coerce y 'float)))


(defun set-vector3 (vec x y z)
  "Set the coordinate values of a 3D vector."
  (setf (vector3.x vec) (coerce x 'float)
        (vector3.y vec) (coerce y 'float)
        (vector3.z vec) (coerce z 'float)))

(defun set-vector4 (vec x y z w)
  "Set the coordinate values of a 4D vector."
  (setf (vector4.x vec) (coerce x 'float)
        (vector4.y vec) (coerce y 'float)
        (vector4.z vec) (coerce z 'float)
        (vector4.w vec) (coerce w 'float)))
|#
(defmacro struct-setter (name &rest skip-fields)
  ;; TODO: Type checking/coercion
  (let ((fields (mapcar #'(lambda (field)
                            (intern (remove #\:
                                            (format nil "~a"
                                                    (autowrap:foreign-type-name field)))))
                        (reverse (autowrap:foreign-record-fields
                                  (autowrap:find-type `(:struct (,name))))))))
    `(defun ,(intern (format nil "SET-~:@a" name)) ,(append '(struct)
                                                     (remove-if #'(lambda (f)
                                                                    (member f skip-fields))
                                                      fields))
       ,@(loop for field in fields
               unless (member field skip-fields)
                 collect (let ((accessor (intern (format nil "~:@a.~:@a" name field))))
                           `(setf (,accessor struct) ,field))))))

(struct-setter vector2)
(struct-setter vector3)
(struct-setter vector4)
(struct-setter texture)
(struct-setter rectangle)
(struct-setter image)
(struct-setter glyph-info image)
(struct-setter color)
(struct-setter shader)
(struct-setter material-map color texture)
(struct-setter matrix)
(struct-setter mesh)
(struct-setter material params shader)
(struct-setter bone-info name)
;(struct-setter transform rotation scale translation) ; Transforms are read-only apparently?
(struct-setter audio-stream)
