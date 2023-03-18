(in-package #:cl-user)
(defpackage claylib/examples/world-screen
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/world-screen)

(defun main ()
  (with-window (:title "raylib [core] example - world screen")
    (let* ((half-text (/ (measure-text "Enemy: 100 / 100" 20) 2.0))
           (camera (make-camera-3d 10 10 10
                                   0 0 0
                                   0 1 0
                                   :fovy 45.0
                                   :projection +camera-perspective+
                                   :mode +camera-third-person+))
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
                               (text2 (make-text
                                       (format nil
                                               "Cube position in screen space coordinates: [~d, ~d]"
                                               (x cube) (y cube))
                                       10 10
                                       :size 20 :color +lime+))
                               (text3 (make-text "Text 2d should always be on top of the cube"
                                                 10 40
                                                 :size 20
                                                 :color +gray+)))))
           (invec (make-vector3 0 2.5 0))
           (outvec (make-vector2 0 0)))
      (disable-cursor)
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (get-world-to-screen-3d invec camera :vec outvec)
          (setf (x (scene-object scene 'text1)) (- (x outvec) half-text)
                (y (scene-object scene 'text1)) (y outvec)
                (text (scene-object scene 'text2))
                (format nil
                        "Cube position in screen space coordinates: [~d, ~d]"
                        (x outvec) (y outvec)))
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene scene '(cube outline grid)))
            (draw-scene scene '(text1 text2 text3))))))))
