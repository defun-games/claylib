(in-package #:cl-user)
(defpackage claylib/examples/audio-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/audio-1)

(defclass circle-wave (circle)
  ((%velocity :initarg :velocity
              :type number
              :accessor velocity)
   (%alpha :initarg :alpha
           :type number
           :accessor alpha)))

(defvar *colors* (list +orange+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+ +pink+
                      +yellow+ +green+ +skyblue+ +purple+ +beige+))

(defun make-circle-wave (x y radius color velocity &key color2 (filled t) (alpha 0))
  (make-instance 'circle-wave
                 :pos (make-vector2 x y)
                 :radius radius
                 :color color
                 :color2 color2
                 :filled filled
                 :velocity velocity
                 :alpha alpha))

(defun random-radius ()
  (+ 10 (random 30.0)))

(defun random-x (radius)
  "Return a random valid X position for a circle with RADIUS."
  (+ radius (random (- (get-screen-width) radius))))

(defun random-y (radius)
  "Return a random valid Y position for a circle with RADIUS."
  (+ radius (random (- (get-screen-height) radius))))

(defun random-velocity ()
  (/ (1+ (random 100)) 2000.0))

(defparameter *scene*
  (let ((bar-x 20)
        (bar-y (- (get-screen-height) 32))
        (bar-w (- (get-screen-width) 40))
        (bar-h 12))
    (make-scene ()
                ((circles (loop for i from 63 downto 0
                                collect (let* ((r (random-radius))
                                               (x (random-x r))
                                               (y (random-y r))
                                               (color (copy-color (alexandria:random-elt *colors*)))
                                               (velocity (random-velocity)))
                                          (make-circle-wave x y r color velocity))))
                 (timebar-bg   (make-rectangle bar-x bar-y bar-w bar-h +lightgray+))
                 (timebar-fill (make-rectangle bar-x bar-y 0     bar-h +maroon+))
                 (timebar-line (make-rectangle bar-x bar-y bar-w bar-h +lightgray+ :filled nil))))))

(defun main ()
  (with-window (:title "raylib [audio] example - module playing (streaming)"
                :flags (list +flag-msaa-4x-hint+))
    (with-audio-device
      (with-music-stream music (claylib/examples:claylib-path "examples/audio/resources/mini1111.xm")
        (play-music-stream music)
        (setf (looping music) nil)
        (with-scenes *scene*
          (with-scene-objects (circles timebar-fill) *scene*
            (do-game-loop (:livesupport t
                           :vars ((pitch 1.0)
                                  (pause nil)))
              (update-music-stream music)

              (when (is-key-pressed-p +key-space+)
                (stop-music-stream music)
                (play-music-stream music))

              (when (is-key-pressed-p +key-p+)
                (if (setf pause (not pause))
                    (pause-music-stream music)
                    (resume-music-stream music)))

              (if (is-key-down-p +key-down+)
                  (decf pitch 0.01)
                  (when (is-key-down-p +key-up+) (incf pitch 0.01)))
              (set-music-pitch music pitch)

              (setf (width timebar-fill) (* (/ (get-music-time-played music)
                                               (get-music-time-length music))
                                            (- (get-screen-width) 40)))

              (loop
                for circle in circles
                do (with-accessors ((a alpha) (r radius) (v velocity) (x x) (y y) (c color)) circle
                     (incf a v)
                     (incf r (* 10 v))
                     (cond ((> a 1)
                            (setf v (- v)))
                           ((<= a 0)
                            (setf a 0
                                  r (random-radius)
                                  x (random-x r)
                                  y (random-y r)
                                  ;; FIXME sadly allocating new colors here, can we avoid this?
                                  c (copy-color (alexandria:random-elt *colors*))
                                  v (random-velocity))))
                     (fade c a)))

              (with-drawing ()
                (draw-scene-all *scene*)))))))))