(in-package #:cl-user)
(defpackage claylib/examples/background-scrolling
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/background-scrolling)

(defparameter *assets*
  (list (make-texture-asset (claylib/examples:claylib-path
                             "examples/textures/resources/cyberpunk_street_background.png"))
        (make-texture-asset (claylib/examples:claylib-path
                             "examples/textures/resources/cyberpunk_street_midground.png"))
        (make-texture-asset (claylib/examples:claylib-path
                             "examples/textures/resources/cyberpunk_street_foreground.png"))))

(defparameter *scene*
  (make-scene ((bg-ass (first *assets*))
               (mg-ass (second *assets*))
               (fg-ass (third *assets*)))
              ((textures (loop for y in '(20 20 70)
                               for ass in (list bg-ass mg-ass fg-ass)
                               for w = (* (width ass) 2)
                               for h = (* (height ass) 2)
                               collect (make-texture ass 0 y :width w :height h)
                               collect (make-texture ass 0 y :width w :height h)))
               (title (make-text "BACKGROUND SCROLLING & PARALLAX" 10 10 :size 20 :color +red+))
               (copyright (make-text "(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)"
                                     (- (get-screen-width) 330) (- (get-screen-height) 20)
                                     :size 10
                                     :color +raywhite+)))))

(defun compute-scrolling (scrolling dec tex)
  "Compute the new scrolling value based on the old SCROLLING, the decrement DEC, and the width of
the associated texture TEX."
  (let ((scrolling (- scrolling dec)))
    (if (< scrolling (- (width tex)))
        0
        scrolling)))

(defun main ()
  (with-window (:title "raylib [textures] example - background scrolling")
    (with-scenes *scene* ()
      (with-scene-objects (textures) *scene*
        (do-game-loop (:livesupport t
                       :vars ((scrolling-back 0 (compute-scrolling scrolling-back
                                                                   0.1
                                                                   (first textures)))
                              (scrolling-mid 0 (compute-scrolling scrolling-mid
                                                                  0.5
                                                                  (third textures)))
                              (scrolling-fore 0 (compute-scrolling scrolling-fore
                                                                   1
                                                                   (fifth textures)))
                              (bg-color (make-color 5 44 70))))
          (loop for (tex1 tex2) on textures by #'cddr
                for scrolling in (list scrolling-back scrolling-mid scrolling-fore)
                do (setf (x tex1) scrolling
                         (x tex2) (+ (width tex1) scrolling)))
          (with-drawing (:bgcolor bg-color)
            (draw-scene-all *scene*)))))))
