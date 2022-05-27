(in-package #:claylib/examples)

(defun example-textures-01 ()
  (with-window (:title "raylib [textures] example - texture loading and drawing")
    (let* ((image-size 256)
           (source-rec (make-rectangle 0 0 image-size image-size +blue+))
           (dest-rec (make-rectangle (truncate (- (get-screen-width) image-size) 2)
                                     (truncate (- (get-screen-height) image-size) 2)
                                     image-size
                                     image-size +red+))
           (tex (make-instance 'texture-asset
                               :path "/home/me/claylib/examples/textures/resources/raylib_logo.png"))
           (scene (make-scene `((texture ,tex))
                              `((texture ,(make-instance 'texture
                                                         :origin (make-vector2 0 0)
                                                         :source source-rec
                                                         :dest dest-rec
                                                         :tint +white+
                                                         :rot 0.0))
                                (text ,(make-text "this IS a texture!"
                                                  360 370
                                                  :size 10
                                                  :color +gray+))))))
      (with-scene scene ()
        (setf (claylib::c-struct (scene-object scene 'texture))
              (claylib::c-asset tex))
        (do-game-loop ()
          (with-drawing
            (draw-scene-all scene)))))))
