(in-package #:cl-user)
(defpackage claylib/examples/core-15
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/core-15)

(defun main ()
  (with-window (:title "raylib [core] example - 3d camera free")
    (let* ((half-text (/ (measure-text "Enemy: 100 / 100" 20) 2.0))
           (camera (make-camera-3d 10 10 10
                                   0 0 0
                                   0 1 0
                                   :fovy 45.0
                                   :projection +camera-perspective+
                                   :mode +camera-free+))
           (scene (make-scene ()
                              ((cube (make-cube 0 0 0
                                                2 2 2
                                                +red+))
                               (outline (make-cube 0 0 0
                                                   2 2 2
                                                   +maroon+
                                                   :filled nil))
                               (grid (make-grid 10 1.0))
                               (text1 (make-text "Enemy: 100 / 100"
                                                 (- half-text)
                                                 0
                                                 :size 20
                                                 :color +black+))
                               (text2 (make-text "Text is always on top of the cube"
                                                 (/ (- *screen-width*
                                                       (measure-text
                                                        "Text is always on top of the cube"
                                                        20))
                                                    2.0)
                                                 25
                                                 :size 20
                                                 :color +gray+)))))
           (invec (make-vector3 0 2.5 0))
           (outvec (make-vector2 0 0)))
      (with-scenes scene
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (get-world-to-screen-3d invec camera :vec outvec)
          (setf (x (scene-object scene 'text1)) (- (x outvec) half-text)
                (y (scene-object scene 'text1)) (y outvec))
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene scene 'cube 'outline 'grid))
            (draw-scene scene 'text1 'text2)))))))
