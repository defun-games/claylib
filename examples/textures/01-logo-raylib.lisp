(in-package #:claylib/examples)

(defun example-textures-01 ()
  (with-window (:title "raylib [textures] example - texture loading and drawing")
    (let* ((image-size 256)
           (source-rec (make-rectangle 0 0 image-size image-size +blue+))
           (dest-rec (make-rectangle (truncate (- (get-screen-width) image-size) 2)
                                     (truncate (- (get-screen-height) image-size) 2)
                                     image-size
                                     image-size
                                     +red+))
           (texture-asset (make-instance 'texture-asset
                                         :path (asdf:system-relative-pathname
                                                :claylib
                                                "examples/textures/resources/raylib_logo.png")))
           (scene (make-scene `((texture ,texture-asset))
                              `((texture ,(make-texture texture-asset source-rec dest-rec))
                                (text ,(make-text "this IS a texture!"
                                                  360 370
                                                  :size 10
                                                  :color +gray+))))))
      (with-scene scene ()
        (do-game-loop ()
          (with-drawing
            (draw-scene-all scene)))))))
