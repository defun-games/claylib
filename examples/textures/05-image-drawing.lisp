(in-package #:cl-user)
(defpackage claylib/examples/textures-5
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-5)

(defvar *assets*
  (list (make-image-asset (asdf:system-relative-pathname
                           :claylib
                           "examples/textures/resources/cat.png"))
        (make-image-asset (asdf:system-relative-pathname
                           :claylib
                           "examples/textures/resources/parrots.png"))
        (make-instance 'font-asset
                       :path (asdf:system-relative-pathname
                              :claylib
                              "examples/textures/resources/custom_jupiter_crash.png"))))

(defun center-texture (texture)
  "Set the TEXTURE's destination rectangle to the center of the screen."
  (let* ((x (- (truncate (get-screen-width) 2)
               (truncate (width texture) 2)))
         (y (-  (truncate (get-screen-height) 2)
                (truncate (height texture) 2)
                40))
         (dest (make-rectangle x y
                               (width texture) (height texture)
                               +white+)))
    (setf (dest texture) dest)))

(defun draw-shapes (image)
  "Draw some shapes on the IMAGE."
  (image-draw-pixel image 10 10 +raywhite+)
  (image-draw-circle image (make-circle 10 10 5 +raywhite+))
  (image-draw-rectangle image (make-rectangle 5 20 10 10 +raywhite+)))

(defun draw-image-on-image (src-image dest-image)
  "Superimpose the DEST-IMAGE onto the SRC-IMAGE."
  (let ((draw-source (make-rectangle 0 0
                                     (width src-image) (height src-image)
                                     +white+))
        (draw-dest (make-rectangle 30 40
                                   (* 1.5 (width src-image)) (* 1.5 (height src-image))
                                   +white+)))
    (image-draw dest-image src-image draw-source draw-dest +white+)))


(defparameter *scene*
  (make-scene ((cat-asset (car *assets*))
               (parrots-asset (cadr *assets*))
               (font-asset (caddr *assets*)))
              ((image (let* ((cat-image (copy-asset-to-object cat-asset))
                             (parrots-image (copy-asset-to-object parrots-asset))
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
                                               :spacing -2)))
                        ;; Process with image manipulation functions
                        (image-crop cat-image cat-crop)
                        (image-flip-horizontal cat-image)
                        (image-resize cat-image 150 200)
                        (draw-image-on-image cat-image parrots-image)
                        (image-crop parrots-image parrot-crop)

                        ;; Draw on the image with a few image draw methods
                        (draw-shapes parrots-image)

                        ;; Draw over image using custom font
                        (image-draw-text-ex parrots-image title)

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
  (with-scene *scene* ()
    (do-game-loop (:livesupport t)
      (with-drawing
        (draw-scene-all *scene*))))))
