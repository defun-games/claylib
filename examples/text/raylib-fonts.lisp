(in-package #:cl-user)
(defpackage claylib/examples/raylib-fonts
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/raylib-fonts)

(defparameter *assets*
  (loop for font in '("alagard" "pixelplay" "mecha" "setback" "romulus" "pixantiqua" "alpha_beta"
                      "jupiter_crash")
        collect (make-font-asset (claylib/examples:claylib-path
                                  (concatenate 'string
                                               "examples/text/resources/fonts/" font ".png")))))

(defparameter *scene*
  (make-scene ((assets *assets*))
              ((fonts (loop for col in (list +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+
                                             +lime+ +gold+ +red+)
                            for ass in assets
                            for txt in (list "ALAGARD FONT designed by Hewett Tsoi"
                                             "PIXELPLAY FONT designed by Aleksander Shevchuk"
                                             "MECHA FONT designed by Captain Falcon"
                                             "SETBACK FONT designed by Brian Kent (AEnigma)"
                                             "ROMULUS FONT designed by Hewett Tsoi"
                                             "PIXANTIQUA FONT designed by Gerhard Grossmann"
                                             "ALPHA_BETA FONT designed by Brian Kent (AEnigma)"
                                             "JUPITER_CRASH FONT designed by Brian Kent (AEnigma)")
                            for spc in (list 2 4 8 4 3 4 4 1)
                            collect (make-text txt 0 0 :color col :font (asset ass) :spacing spc)))
               (text (make-text "free fonts included with raylib"
                                250 20
                                :size 20
                                :color +darkgray+))
               (line (make-line-2d 220 50 590 50 +darkgray+)))))

(defun arrange-text (texts)
  "Set sizes & positions for each text in TEXTS so they are arranged in order on the screen."
  (loop for text in texts
        for i from 0
        do (setf (size text) (* 2 (size (font text)))
                 (x text) (- (/ (get-screen-width) 2)
                             (/ (x (measure-text-ex text)) 2))
                 (y text) (+ 60 (size (font text)) (* 45 i)))
           (incf (y text) (case i
                            (3 8)
                            (4 2)
                            (7 -8)
                            (t 0)))))

(defun main ()
  (with-window (:title "raylib [text] example - raylib fonts")
    (with-scenes *scene* ()
      (arrange-text (scene-object *scene* 'fonts))
      (do-game-loop (:livesupport t)
        (with-drawing ()
          (draw-scene-all *scene*))))))
