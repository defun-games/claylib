(in-package #:cl-user)
(defpackage claylib/examples/2d-camera
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/2d-camera)


(defun main ()
  (with-window (:title "raylib [core] example - 2d camera")
    (let* ((camera (make-camera-2d (/ *screen-width* 2.0)
                                   (/ *screen-height* 2.0)
                                   420
                                   300))
           (scene
             (make-scene ()
                         ((ground (make-rectangle -6000 320 13000 8000 +darkgray+))
                          (player (make-rectangle 400 280 40 40 +red+))
                          (title (make-text "SCREEN AREA" 640 10 :size 20 :color +red+))
                          (r1 (make-rectangle 0 0 *screen-width* 5 +red+))
                          (r2 (make-rectangle 0 5 5 (- *screen-height* 10) +red+))
                          (r3 (make-rectangle (- *screen-width* 5) 5 5 (- *screen-height* 10) +red+))
                          (r4 (make-rectangle 0 (- *screen-height* 5) *screen-width* 5 +red+))
                          (r5 (make-rectangle 10 10 250 113 (fade +skyblue+ 0.5 t)))
                          (r6 (make-rectangle 10 10 250 113 +blue+ :filled nil))
                          (t1 (make-text "Free 2d camera controls:"
                                         20 20
                                         :size 10 :color +black+))
                          (t2 (make-text "- Right/Left to move Offset"
                                         40 40
                                         :size 10 :color +darkgray+))
                          (t3 (make-text "- Mouse Wheel to Zoom in-out"
                                         40 60
                                         :size 10 :color +darkgray+))
                          (t4 (make-text "- A / S to Rotate"
                                         40 80
                                         :size 10 :color +darkgray+))
                          (t5 (make-text "- R to reset Zoom and Rotation"
                                         40 100
                                         :size 10 :color +darkgray+))
                          (vline (make-line-2d (x (target camera)) (* *screen-height* -10)
                                               (x (target camera)) (* *screen-height* 10)
                                               +green+))
                          (hline (make-line-2d (* *screen-width* -10) (y (target camera))
                                               (* *screen-width* 10) (y (target camera))
                                               +green+))))))
      (loop with spacing = 0
            repeat 100
            do (let ((w (get-random-value 50 200))
                     (h (get-random-value 100 800)))
                 (setf (gethash (gensym "BUILDING") (objects scene))
                       (make-rectangle (- spacing 6000.0)
                                       (- *screen-height* 130.0 h)
                                       w
                                       h
                                       (make-color (get-random-value 200 240)
                                                   (get-random-value 200 240)
                                                   (get-random-value 200 250))))
                 (incf spacing w)))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (when (is-key-down-p +key-right+) (incf (x (scene-object scene 'player)) 2))
          (when (is-key-down-p +key-left+) (decf (x (scene-object scene 'player)) 2))
          (setf (x (target camera)) (+ (x (scene-object scene 'player)) 20)
                (y (target camera)) (+ (y (scene-object scene 'player)) 20)
                (x1 (scene-object scene 'vline)) (x (target camera))
                (x2 (scene-object scene 'vline)) (x (target camera))
                (y1 (scene-object scene 'hline)) (y (target camera))
                (y2 (scene-object scene 'hline)) (y (target camera)))
          (when (and (is-key-down-p +key-a+) (>= (rot camera) -40))
            (decf (rot camera)))
          (when (and (is-key-down-p +key-s+) (<= (rot camera) 40))
            (incf (rot camera)))
          (setf (zoom camera)
                (min 3.0 (max 0.1 (+ (zoom camera) (* 0.05 (get-mouse-wheel-move))))))
          (when (is-key-pressed-p +key-r+)
            (setf (zoom camera) 1.0
                  (rot camera) 0.0))
          (with-drawing ()
            (with-2d-mode camera
              (draw-scene scene 'ground)
              (draw-scene-regex scene "^BUILDING")
              (draw-scene scene '(player vline hline)))
            (draw-scene scene
                        '(title r1 r2 r3 r4 r5 r6
                          t1 t2 t3 t4 t5))))))))
