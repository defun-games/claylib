(in-package #:cl-user)
(defpackage claylib/examples/module-playing
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/module-playing)

(defclass circle-wave (circle)
  ((%velocity :initarg :velocity
              :type number
              :accessor velocity)
   (%alpha :initarg :alpha
           :type number
           :accessor alpha)))

(defun make-circle-wave (x y radius color velocity &key (alpha 0))
  (make-instance 'circle-wave
                 :pos (make-vector2 x y)
                 :radius radius
                 :color color
                 :velocity velocity
                 :alpha alpha))

(defvar *colors* (list +orange+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+ +pink+
                      +yellow+ +green+ +skyblue+ +purple+ +beige+))

(defun random-bounded (lower upper)
  "Return a random value ∈ [LOWER, UPPER] (inclusive)."
  (+ lower (random (1+ (- upper lower)))))

(defun random-x (radius)
  "Return a random valid X position for a circle with RADIUS."
  (+ radius (random (- (get-screen-width) radius))))

(defun random-y (radius)
  "Return a random valid Y position for a circle with RADIUS."
  (+ radius (random (- (get-screen-height) radius))))

(defun random-velocity ()
  (/ (1+ (random 100)) 2000.0))

(defparameter *scene*
  (make-scene-pro ((:assets (music-ass (make-music-asset (claylib/examples::claylib-path
                                                          "examples/audio/resources/mini1111.xm"))))
                   (:params (bar-x 20)
                            (bar-y (- (get-screen-height) 32))
                            (bar-w (- (get-screen-width) 40))
                            (bar-h 12)
                            (music (asset music-ass)))
                   (:objects (circles (loop for i from 63 downto 0
                                            collect (let* ((r (random-bounded 10 30))
                                                           (x (random-x r))
                                                           (y (random-y r))
                                                           (color (copy-color
                                                                   (alexandria:random-elt *colors*)))
                                                           (velocity (random-velocity)))
                                                      (make-circle-wave x y r color velocity))))
                             (timebar-bg   (make-rectangle bar-x bar-y bar-w bar-h +lightgray+))
                             (timebar-fill (make-rectangle bar-x bar-y 0     bar-h +maroon+))
                             (timebar-line
                              (make-rectangle bar-x bar-y bar-w bar-h +gray+ :filled nil))))))

(defun main ()
  (with-window (:title "raylib [audio] example - module playing (streaming)"
                :flags (list +flag-msaa-4x-hint+))
    (with-scenes *scene* ()
      (let ((music (scene-param *scene* 'music)))
        (play music)
        (setf (looping music) nil)
        (with-scene-objects (circles timebar-fill) *scene*
          (do-game-loop (:livesupport t
                         :vars ((pitch 1.0)
                                (pause nil)))
            (update music)

            (when (is-key-pressed-p +key-space+)
              (stop music)
              (play music))

            (when (is-key-pressed-p +key-p+)
              (if (setf pause (not pause))
                  (pause music)
                  (resume music)))

            (if (is-key-down-p +key-down+)
                (decf pitch 0.01)
                (when (is-key-down-p +key-up+) (incf pitch 0.01)))
            (setf (pitch music) pitch)

            ;; FIXME: GET-MUSIC-TIME-PLAYED does not reset to 0 when a .xm file is restarted.
            ;; Unknown if this is a bug in Raylib or simply unsupported for that format.
            (setf (width timebar-fill) (* (min (/ (get-music-time-played music)
                                                  (get-music-time-length music))
                                               1)
                                          (scene-param *scene* 'bar-w)))

            (loop
              for circle in circles
              do (with-accessors ((a alpha) (r radius) (v velocity) (x x) (y y) (c color)) circle
                   (incf a v)
                   (incf r (* 10 v))
                   (cond ((> a 1)
                          (setf v (- v)))
                         ((<= a 0)
                          (setf a 0
                                r (random-bounded 10 30)
                                x (random-x r)
                                y (random-y r)
                                v (random-velocity))
                          (copy-color (alexandria:random-elt *colors*) c)))
                   (fade c a)))

            (with-drawing ()
              (draw-scene-all *scene*))))))))
