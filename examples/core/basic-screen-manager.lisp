(in-package #:cl-user)
(defpackage claylib/examples/basic-screen-manager
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/basic-screen-manager)

(defun make-screen (color)
  (make-rectangle 0 0 *screen-width* *screen-height* color))

(defparameter *logo* (make-scene () ((paint (make-screen *claylib-background*))
                                     (text (make-text "LOGO SCREEN" 20 20
                                                      :size 40 :color +lightgray+))
                                     (subtext (make-text "WAIT for 2 SECONDS..." 290 220
                                                         :size 20 :color +gray+)))))

(defparameter *title* (make-scene () ((paint (make-screen +green+))
                                      (text (make-text "TITLE SCREEN" 20 20
                                                       :size 40 :color +darkgreen+))
                                      (subtext (make-text "PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN"
                                                          120 220
                                                          :size 20 :color +darkgreen+)))))

(defparameter *gameplay* (make-scene () ((paint (make-screen +purple+))
                                         (text (make-text "GAMEPLAY SCREEN" 20 20
                                                          :size 40 :color +maroon+))
                                         (subtext (make-text "PRESS ENTER or TAP to JUMP to ENDING SCREEN"
                                                             130 220
                                                             :size 20 :color +maroon+)))))

(defparameter *ending* (make-scene () ((paint (make-screen +blue+))
                                       (text (make-text "ENDING SCREEN" 20 20
                                                        :size 40 :color +darkblue+))
                                       (subtext (make-text "PRESS ENTER or TAP to RETURN to TITLE SCREEN"
                                                           120 220
                                                           :size 20 :color +darkblue+)))))

(defun next-screen ()
  "Is user indicating we can move to the next stage?"
  (or
   (is-key-pressed-p +key-enter+)
   (is-gesture-detected-p +gesture-tap+)))

(defun main ()
  (with-window (:title "raylib [core] example - basic screen manager")
    (with-scenes (list *logo* *title* *gameplay* *ending*) ()
      (do-game-loop (:livesupport t
                     :vars ((scenes `(,*logo* ,@(alexandria:circular-list *title* *gameplay* *ending*)))
                            (frame-count 0)))
        (incf frame-count)
        (when (or (and (next-screen) (not (eql (car scenes) *logo*)))
                  (and (eql (car scenes) *logo*) (> frame-count (* 2 *target-fps*))))
          (setf scenes (cdr scenes)))
        (with-drawing ()
          (draw-scene-all (car scenes)))))))
