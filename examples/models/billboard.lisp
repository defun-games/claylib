(in-package #:cl-user)
(defpackage claylib/examples/billboard
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/billboard)

(defparameter *scene*
  (make-scene-pro ((:assets (bill-ass (make-texture-asset
                                       (claylib/examples:claylib-path
                                        "examples/models/resources/billboard.png"))))
                   (:params (cam (make-camera-3d 5 4 5
                                                 0 2 0
                                                 0 1 0
                                                 :mode +camera-orbital+))
                            (source (make-simple-rec 0 0
                                                      (width bill-ass) (height bill-ass))))
                   (:objects (grid (make-grid 10 1))
                             (bill1 (make-billboard bill-ass
                                                    cam
                                                    0 2 0
                                                    4 4
                                                    source))
                             (bill2 (make-billboard bill-ass
                                                    cam
                                                    1 2 1
                                                    2 2
                                                    source))))))

(defun main ()
  (with-window (:title "raylib [models] example - drawing billboards")
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t)
        (with-scene-bindings () *scene*
          (incf (rot-angle bill2) 0.4)
          (update-camera cam)
          (with-drawing ()
            (with-3d-mode cam
              (draw-scene *scene* 'grid)
              (if (> (vector3-distance (pos cam) (pos bill1))
                     (vector3-distance (pos cam) (pos bill2)))
                  (draw-scene *scene* '(bill1 bill2))
                  (draw-scene *scene* '(bill2 bill1))))
            (draw-fps 10 10)))))))
