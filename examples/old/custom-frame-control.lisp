(in-package #:cl-user)
(defpackage claylib/examples/custom-frame-control
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/custom-frame-control)

(defun main ()
  (with-window (:title "raylib [core] example - custom frame control")
    (let ((scene
            (make-scene ()
                        ((ball (make-circle 0
                                            (- (/ (get-screen-height) 2) 25)
                                            50
                                            +red+))
                         (text-timer (make-text "0"
                                                30
                                                (- (/ (get-screen-height) 2) 100)
                                                :size 20
                                                :color +maroon+))
                         (text-pos-x (make-text "10"
                                                -40
                                                (+ (/ (get-screen-height) 2) 40)
                                                :size 20
                                                :color +black+))
                         (text-info (make-text "Circle is moving at a constant 200 pixels/sec,
independently of the frame rate."
                                               10
                                               10
                                               :size 20
                                               :color +darkgray+))
                         (text-space (make-text "PRESS SPACE to PAUSE MOVEMENT"
                                                10
                                                (- (get-screen-height) 60)
                                                :size 20
                                                :color +gray+))
                         (text-change-fps (make-text "PRESS UP | DOWN to CHANGE TARGET FPS"
                                                     10
                                                     (- (get-screen-height) 30)
                                                     :size 20
                                                     :color +gray+))
                         (text-target-fps (make-text "TARGET FPS:"
                                                     (- (get-screen-width) 220)
                                                     10
                                                     :size 20
                                                     :color +lime+))
                         (text-current-fps (make-text "CURRENT FPS:"
                                                      (- (get-screen-width) 220)
                                                      40
                                                      :size 20
                                                      :color +green+))))))
      (loop for i = 0 then (1+ i)
            for name = (gensym "LINE")
            until (> i (/ (get-screen-width) 200))
            do (setf (gethash name (objects scene))
                     (make-rectangle (* 200 i) 0
                                     1 (get-screen-height)
                                     +skyblue+)))
      (with-scenes scene
        (do-game-loop (:livesupport t
                       :vars ((previous-time (get-time) current-time)
                              (current-time 0.0)
                              (update-draw-time 0.0)
                              (wait-time 0.0)
                              (delta-time 0.0)
                              (time-counter 0.0)
                              (pause nil)
                              (target-fps 60)))
          ;; Update
          ;; ---------------------------------
          (when (is-key-pressed-p +key-space+)
            (setf pause (not pause)))

          (if (is-key-pressed-p +key-up+)
              (incf target-fps 20)
              (when (is-key-pressed-p +key-down+)
                (decf target-fps 20)))
          (when (< target-fps 0)
            (setf target-fps 0))

          (unless pause
            (with-scene-objects (ball text-timer text-pos-x text-target-fps text-current-fps) scene
              (when (>= (incf (x ball) (coerce (* 200 delta-time) 'single-float))
                        (get-screen-width))
                (setf (x ball) 0))
              (setf (text text-timer) (format nil "~,0f ms" (* time-counter 1000))
                    (x text-timer) (- (x ball) 40)
                    (y text-timer) (- (/ (get-screen-height) 2) 100)
                    (text text-pos-x) (format nil "PosX: ~,0f" (x ball))
                    (x text-pos-x) (- (x ball) 50)
                    (y text-pos-x) (+ (/ (get-screen-height) 2) 40)
                    (text text-target-fps) (format nil "TARGET FPS: ~d" target-fps)
                    (text text-current-fps) (format nil
                                                    "CURRENT FPS: ~d"
                                                    (if (= delta-time 0)
                                                        0
                                                        (floor (/ 1 delta-time)))))
              (incf time-counter delta-time)))

          ;; Draw
          ;; ---------------------------------
          (with-drawing ()
            (draw-scene-all scene))

          (swap-screen-buffer)

          (setf current-time (get-time))
          (setf update-draw-time (- current-time previous-time))

          (if (> target-fps 0)
              (when (> (setf wait-time (- (/ 1.0 target-fps) update-draw-time)) 0)
                (wait-time (coerce wait-time 'double-float))
                (setf current-time (get-time))
                (setf delta-time (- current-time previous-time)))
              (setf delta-time update-draw-time)))))))
