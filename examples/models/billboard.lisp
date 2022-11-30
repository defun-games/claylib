(in-package #:cl-user)
(defpackage claylib/examples/billboard
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/billboard)

(defparameter *scene*
  (make-scene-pro ((:params (cam (make-camera-3d 5 4 5
                                                 0 2 0
                                                 0 1 0
                                                 :mode +camera-orbital+)))
                   (:assets (bill-ass (make-texture-asset
                                       (claylib/examples:claylib-path
                                        "examples/models/resources/billboard.png"))))
                   (:objects (grid (make-grid 10 1))
                             (bill (make-billboard bill-ass
                                                   cam
                                                   0 2 0
                                                   2 2
                                                   (make-simple-rec 0 0
                                                                    (width bill-ass) (height bill-ass))))))))

(defun main ()
  (with-window (:title "raylib [models] example - drawing billboards")
    (with-scenes *scene* ()
      (let ((cam (scene-param *scene* 'cam)))
        (do-game-loop (:livesupport t)
          (update-camera cam)
          (with-drawing ()
            (with-3d-mode cam (draw-scene-all *scene*))
            (draw-fps 10 10)))))))
