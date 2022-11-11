(in-package #:cl-user)
(defpackage claylib/examples/srcrec-dstrec
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/srcrec-dstrec)

(defun main ()
  (with-window (:title "raylib [textures] example - texture source and destination rectangles")
    (let* ((scarfy-asset (make-texture-asset (claylib/examples:claylib-path
                                              "examples/textures/resources/scarfy.png")
                                             :load-now t))
           (frame-width (/ (width scarfy-asset) 6))
           (frame-height (height scarfy-asset))
           (source-rec (make-simple-rec 0 0 frame-width frame-height))
           (screen-midpoint (make-vector2 (/ (get-screen-width) 2)
                                          (/ (get-screen-height) 2)))
           (scene (make-scene ()
                              ((y-axis (make-line-2d (x screen-midpoint)
                                                     0
                                                     (x screen-midpoint)
                                                     (get-screen-height)
                                                     +gray+))
                               (x-axis (make-line-2d 0
                                                     (y screen-midpoint)
                                                     (get-screen-width)
                                                     (y screen-midpoint)
                                                     +gray+))
                               (scarfy (make-texture scarfy-asset
                                                     (x screen-midpoint)
                                                     (y screen-midpoint)
                                                     :width (* frame-width 2)
                                                     :height (* frame-height 2)
                                                     :origin (make-vector2 frame-width
                                                                           frame-height)
                                                     :source source-rec
                                                     :rot 0))
                               (copyright-text (make-text "(c) Scarfy sprite by Eiden Marsal"
                                                          (- (get-screen-width) 200)
                                                          (- (get-screen-height) 20)
                                                          :size 10
                                                          :color +gray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (incf (rot (scene-object scene 'scarfy)))

          (with-drawing ()
            (draw-scene-all scene)))))))
