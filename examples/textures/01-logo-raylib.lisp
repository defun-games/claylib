(in-package #:cl-user)
(defpackage claylib/examples/textures-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-1)

(defun main ()
  (with-window (:title "raylib [textures] example - texture loading and drawing")
    (let* ((image-size 256)
           (scene (make-scene ((texass (make-texture-asset
                                        (asdf:system-relative-pathname
                                         :claylib
                                         "examples/textures/resources/raylib_logo.png"))))
                              ((texture (make-texture texass
                                                      (/ (- (get-screen-width) image-size) 2.0)
                                                      (/ (- (get-screen-height) image-size) 2.0)))
                               (text (make-text "this IS a texture!"
                                                360 370
                                                :size 10
                                                :color +gray+))))))
      (with-scenes scene
        (do-game-loop (:livesupport t)
          (with-drawing
            (draw-scene-all scene)))))))
