(in-package #:cl-user)
(defpackage claylib/examples/font-spritefont
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/font-spritefont)

(defparameter *assets*
  (loop for font in '("mecha" "alagard" "jupiter_crash")
        collect (make-font-asset (claylib/examples:claylib-path
                                  (concatenate 'string
                                               "examples/text/resources/custom_" font ".png")))))

(defparameter *scene*
  (make-scene ((assets *assets*))
              ((fonts (loop for ass in assets
                            for spc in (list -3 -2 2)
                            for txt in (list "THIS IS A custom SPRITE FONT..."
                                             "...and this is ANOTHER CUSTOM FONT..."
                                             "...and a THIRD one! GREAT! :D")
                            collect (make-text txt 0 0
                                               :color +white+
                                               :font (asset ass)
                                               :spacing spc
                                               :size (size ass)))))))

(defun arrange-text (texts)
  "Set sizes & positions for each text in TEXTS so they are arranged in order on the screen."
  (loop for text in texts
        for y-offset in (list -80 -10 50)
        do (setf (x text) (- (/ (get-screen-width) 2)
                             (/ (x (measure-text-ex text)) 2))
                 (y text) (- (/ (get-screen-height) 2)
                             (/ (size text) 2)
                             (- y-offset)))))

(defun main ()
  (with-window (:title "raylib [text] example - sprite font loading")
    (with-scenes *scene* ()
      (arrange-text (scene-object *scene* 'fonts))
      (do-game-loop (:livesupport t)
        (with-drawing ()
          (draw-scene-all *scene*))))))
