(in-package #:cl-user)
(defpackage claylib/examples/scissor-test
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/scissor-test)

(defun main ()
  (with-window (:title "raylib [core] example - scissor test")
    (let ((scissor-area (make-instance 'rl-rectangle :x 0 :y 0 :width 300 :height 300))
          (scissor-mode t)
          (scene (make-scene ()
                             ((rect (make-rectangle 0 0
                                                    (get-screen-width) (get-screen-height)
                                                    +red+))
                              (outline (make-rectangle 0 0 300 300 +black+
                                                       :filled nil :thickness 1))
                              (text1 (make-text "Move the mouse around to reveal this text!"
                                                190 200
                                                :size 20 :color +lightgray+))
                              (text2 (make-text "Press S to toggle scissor test"
                                                10 10
                                                :size 20 :color +black+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (when (is-key-pressed-p +key-s+) (setf scissor-mode (not scissor-mode)))
          (setf (x scissor-area) (- (get-mouse-x) (/ (width scissor-area) 2))
                (y scissor-area) (- (get-mouse-y) (/ (height scissor-area) 2))
                (width (scene-object scene 'rect)) (get-screen-width)
                (height (scene-object scene 'rect)) (get-screen-height))
          (let ((outline (scene-object scene 'outline)))
            (setf (x outline) (x scissor-area)
                  (y outline) (y scissor-area)
                  (width outline) (width scissor-area)
                  (height outline) (height scissor-area)))
          (with-drawing ()
            (if scissor-mode
                (with-scissor-mode (truncate (x scissor-area)) (truncate (y scissor-area))
                    (truncate (width scissor-area)) (truncate (height scissor-area))
                  (draw-scene scene '(rect text1)))
                (draw-scene scene '(rect text1)))
            (draw-scene scene '(outline text2))))))))
