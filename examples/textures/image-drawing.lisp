(in-package #:cl-user)
(defpackage claylib/examples/image-drawing
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/image-drawing)

(defvar *assets*
  (list (make-image-asset (claylib/examples:claylib-path "examples/textures/resources/cat.png"))
        (make-image-asset (claylib/examples:claylib-path "examples/textures/resources/parrots.png"))
        (make-font-asset (claylib/examples:claylib-path
                          "examples/textures/resources/custom_jupiter_crash.png"))))

(defun center-texture (texture)
  "Set the TEXTURE's destination rectangle to the center of the screen."
  (let* ((x (- (truncate (get-screen-width) 2)
               (truncate (width texture) 2)))
         (y (-  (truncate (get-screen-height) 2)
                (truncate (height texture) 2)
                40))
         (dest (make-simple-rec x y (width texture) (height texture))))
    (setf (dest texture) dest)))

(defun draw-shapes (image)
  "Draw some shapes on the IMAGE."
  (image-draw image (make-pixel 10 10 +raywhite+))
  (image-draw image (make-circle 10 10 5 +raywhite+))
  (image-draw image (make-rectangle 5 20 10 10 +raywhite+)))

(defparameter *scene*
  (make-scene ((cat-asset (car *assets*))
               (parrots-asset (cadr *assets*))
               (font-asset (caddr *assets*)))
              ((image (let* ((cat-image (make-image cat-asset
                                                    (make-rectangle 0 0 150 200 +white+)
                                                    (make-rectangle 30 40 225 300 +white+)))
                             (parrots-image (make-simple-image parrots-asset))
                             (cat-crop (make-rectangle 100 10 280 380 +white+))
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
                                               :spacing -2
                                               :color +white+)))
                        ;; Process with image manipulation functions
                        (image-crop cat-image cat-crop)
                        (image-flip-horizontal cat-image)
                        (image-resize cat-image 150 200)
                        (image-draw parrots-image cat-image)
                        (image-crop parrots-image parrot-crop)

                        ;; Draw on the image with a few image draw methods
                        (draw-shapes parrots-image)

                        ;; Draw over image using custom font
                        (image-draw parrots-image title)

                        (load-texture-from-image parrots-image
                                                 :texture texture)
                        (center-texture texture)
                        texture))
               (text1 (make-text "We are drawing only one texture from various images composed!"
                                 240 350
                                 :size 10
                                 :color +darkgray+))
               (text2 (make-text "Source images have been cropped, scaled, flipped and copied one over the other."
                                 190 370
                                 :size 10
                                 :color +darkgray+)))))

(defun main ()
  (with-window (:title "raylib [textures] example - image drawing")
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t)
        (with-drawing ()
          (draw-scene-all *scene*))))))
