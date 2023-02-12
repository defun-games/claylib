(in-package #:cl-user)
(defpackage claylib/examples/font-loading
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/font-loading)

(defparameter *assets*
  (list (make-font-asset (claylib/examples:claylib-path "examples/text/resources/pixantiqua.fnt"))
        (make-font-asset (claylib/examples:claylib-path "examples/text/resources/pixantiqua.ttf")
                         :size 32
                         :glyph-count 250)))

(defparameter *scene*
  (let ((msg "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI
JKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn
opqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ
ÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷
øùúûüýþÿ"))
    (make-scene ((bm-ass (car *assets*))
                 (ttf-ass (cadr *assets*)))
                ((instructions (make-text "Hold SPACE to use TTF generated font"
                                          20 20
                                          :size 20
                                          :color +lightgray+))
                 (msg-bm (make-text msg 20 100
                                    :font (asset bm-ass)
                                    :size (size bm-ass)
                                    :spacing 2
                                    :color +maroon+))
                 (title-bm (make-text "Using BMFont (Angelcode) imported"
                                      20 (- (get-screen-height) 30)
                                      :size 20))
                 (msg-ttf (make-text msg 20 100
                                     :font (asset bm-ass)
                                     :size (size bm-ass)
                                     :spacing 2
                                     :color +lime+))
                 (title-ttf (make-text "Using TTF font generated"
                                       20 (- (get-screen-height) 30)
                                       :size 20))))))

(defun main ()
  (with-window (:title "raylib [text] example - font loading")
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t)
        (with-drawing ()
          (if (is-key-down-p +key-space+)
              (draw-scene-except *scene* '(msg-bm title-bm))
              (draw-scene-except *scene* '(msg-ttf title-ttf))))))))
