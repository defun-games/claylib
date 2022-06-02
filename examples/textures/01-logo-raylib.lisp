(in-package #:cl-user)
(defpackage claylib/examples/textures-1
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-1)

(defun main ()
  (with-window (:title "raylib [textures] example - texture loading and drawing")
    (let* ((image-size 256)
           (texture-asset (make-instance 'texture-asset
                                         :path (asdf:system-relative-pathname
                                                :claylib
                                                "examples/textures/resources/raylib_logo.png")))
           (scene (make-scene `((texture ,texture-asset))
                              `((texture ,(make-texture texture-asset
                                                        (/ (- (get-screen-width) image-size) 2.0)
                                                        (/ (- (get-screen-height) image-size) 2.0)))
                                (text ,(make-text "this IS a texture!"
                                                  360 370
                                                  :size 10
                                                  :color +gray+))))))
      (with-scene scene ()
        (do-game-loop ()
          (with-drawing
            (draw-scene-all scene)))))))
