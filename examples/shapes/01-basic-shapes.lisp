(in-package #:cl-user)
(defpackage claylib/examples/shapes-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/shapes-1)

(defun main ()
  (with-window (:title "raylib [shapes] example - Draw basic shapes 2d (rectangle, circle, line...)")
    (let ((scene (make-scene ()
                             `((text-title ,(make-text "some basic shapes available on raylib"
                                                       20
                                                       20
                                                       :size 20
                                                       :color +darkgray+))
                               (circle ,(make-circle (/ (get-screen-width) 5)
                                                     120
                                                     35
                                                     +darkblue+))
                               (circle-gradient ,(make-circle (/ (get-screen-width) 5)
                                                              220
                                                              60
                                                              +green+
                                                              :color2 +skyblue+))
                               (circle-lines ,(make-circle (/ (get-screen-width) 5)
                                                           340
                                                           80
                                                           +darkblue+
                                                           :filled nil))
                               (rect ,(make-rectangle (- (/ (get-screen-width) 2) 60)
                                                      100
                                                      120
                                                      60
                                                      +red+))
                               (rect-gradient ,(make-rectangle (- (/ (get-screen-width) 2) 90)
                                                               170
                                                               180
                                                               130
                                                               +maroon+
                                                               :color2 +gold+
                                                               :gradient-style :h))
                               (rect-lines ,(make-rectangle (- (/ (get-screen-width) 2) 40)
                                                            320
                                                            80
                                                            60
                                                            +orange+
                                                            :filled nil))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t)
          (with-drawing
            (draw-scene-all scene)))))))
