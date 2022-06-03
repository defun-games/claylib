(in-package #:cl-user)
(defpackage claylib/examples/textures-4
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-4)

(defun main ()
  (with-window (:title "raylib [textures] example - texture source and destination rectangles")
    (let* ((scarfy-asset (make-texture-asset (asdf:system-relative-pathname
                                              :claylib
                                              "examples/textures/resources/scarfy.png")
                                             :load-now t))
           (frame-width (/ (width scarfy-asset) 6))
           (frame-height (height scarfy-asset))
           (source-rec (make-rectangle 0 0
                                       frame-width frame-height
                                       +black+))
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
      (do-game-loop (:livesupport t)
        (incf (rot (scene-object scene 'scarfy)))

        (with-drawing
          (draw-scene-all scene))))))
