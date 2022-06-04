(in-package #:cl-user)
(defpackage claylib/examples/textures-5
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-5)


(defun main ()
  (with-window (:title "raylib [textures] example - image drawing")
    (let ((scene (make-scene
                  ((cat-asset (make-image-asset (asdf:system-relative-pathname
                                                 :claylib
                                                 "examples/textures/resources/cat.png")))
                   (parrots-asset (make-image-asset (asdf:system-relative-pathname
                                                     :claylib
                                                     "examples/textures/resources/parrots.png")))
                   (font-asset (make-instance 'font-asset
                                              :path (asdf:system-relative-pathname
                                                     :claylib
                                                     "examples/textures/resources/custom_jupiter_crash.png"))))
                  ((image (let* ((cat-image (copy-asset-to-object cat-asset))
                                 (parrots-image (copy-asset-to-object parrots-asset))
                                 (cat-crop (make-rectangle 100 10 280 380 +white+))
                                 (draw-source (make-rectangle 0 0
                                                              (width cat-image)
                                                              (height cat-image)
                                                              +green+))
                                 (draw-dest (make-rectangle 30 40
                                                            (* 1.5 (width cat-image))
                                                            (* 1.5 (height cat-image))
                                                            +green+))
                                 (parrot-crop (make-rectangle 0 50
                                                              (width parrots-image)
                                                              (- (height parrots-image) 100)
                                                              +green+))
                                 (texture (make-empty-texture :origin (make-vector2 0 0)
                                                              :rot 0.0
                                                              :tint +white+))
                                 (title (make-text "PARROTS & CAT"
                                                   300 230
                                                   :font (asset font-asset)
                                                   :spacing -2)))
                            ;; Process with image manipulation functions
                            (image-crop cat-image cat-crop)
                            (image-flip-horizontal cat-image)
                            (image-resize cat-image 150 200)
                            (setf (width draw-source) (width cat-image)
                                  (height draw-source) (height cat-image)
                                  (width draw-dest) (* 1.5 (width cat-image))
                                  (height draw-dest) (* 1.5 (height cat-image)))
                            (image-draw parrots-image
                                        cat-image
                                        draw-source
                                        draw-dest
                                        +white+)
                            (image-crop parrots-image parrot-crop)

                            ;; Draw on the image with a few image draw methods
                            (image-draw-pixel parrots-image 10 10 +raywhite+)
                            (image-draw-circle parrots-image (make-circle 10 10 5 +raywhite+))
                            (image-draw-rectangle parrots-image
                                                  (make-rectangle 5 20 10 10 +raywhite+))

                            ;; Draw over image using custom font
                            (image-draw-text-ex parrots-image title)

                            (let* ((half-screen-width (truncate (get-screen-width) 2))
                                   (half-screen-height (truncate (get-screen-height) 2))
                                   (half-texture-width (truncate (width parrots-image) 2))
                                   (half-texture-height (truncate (height parrots-image) 2))
                                   (x (- half-screen-width half-texture-width))
                                   (y (- half-screen-height half-texture-height 40))
                                   (dest (make-rectangle x y
                                                         (width parrots-image)
                                                         (height parrots-image)
                                                         +white+)))
                              (setf (dest texture) dest))

                            ;; Convert to texture
                            (load-texture-from-image parrots-image
                                                     :texture texture)))
                   (text1 (make-text "We are drawing only one texture from various images composed!"
                                     240 350
                                     :size 10
                                     :color +darkgray+))
                   (text2 (make-text "Source images have been cropped, scaled, flipped and copied one over the other."
                                     190 370
                                     :size 10
                                     :color +darkgray+))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t)
          (with-drawing
            (draw-scene-all scene)))))))
