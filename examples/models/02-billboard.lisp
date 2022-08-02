(in-package #:cl-user)
(defpackage claylib/examples/models-2
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/models-2)

(defparameter *assets*
  (list (make-texture-asset (claylib/examples:claylib-path
                             "examples/models/resources/billboard.png"))))

(defparameter *scene*
  (make-scene ((bill-ass (car *assets*)))
              ((grid (make-grid 10 1))
               (bill (make-billboard bill-ass
                                     *cam*
                                     0 2 0
                                     2 2
                                     (make-rectangle 0 0
                                                     (width bill-ass) (height bill-ass)
                                                     +white+))))))

(defun main ()
  (with-window (:title "raylib [models] example - drawing billboards")
    (let ((*cam* (make-camera-3d 5 4 5
                                 0 2 0
                                 0 1 0
                                 :mode +camera-orbital+)))
      (declare (special *cam*))
      (with-scenes *scene*
        (do-game-loop (:livesupport t)
          (update-camera *cam*)
          (with-drawing
            (with-3d-mode *cam* (draw-scene-all *scene*))
            (draw-fps 10 10)))))))
