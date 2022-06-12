(in-package #:cl-user)
(defpackage claylib/examples/core-2
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/core-2)

(defun next-screen ()
   "Is user indicating we can move to the next stage?"
  (or
   (is-key-pressed-p +key-enter+)
   (is-gesture-detected-p +gesture-tap+)))

(defun make-screen (color)
  (make-rectangle 0 0 *screen-width* *screen-height* color))

(defun main ()
  (with-window (:title "raylib [core] example - basic screen manager")
    (let ((logo (make-scene () ((paint (make-screen *claylib-background*))
                                (text (make-text "LOGO SCREEN" 20 20
                                                 :size 40 :color +lightgray+))
                                (subtext (make-text "WAIT for 2 SECONDS..." 290 220
                                                    :size 20 :color +gray+)))))
          (title (make-scene () ((paint (make-screen +green+))
                                 (text (make-text "TITLE SCREEN" 20 20
                                                  :size 40 :color +darkgreen+))
                                 (subtext (make-text "PRESS ENTER or TAP to JUMP to GAMEPLAY SCREEN"
                                                     120 220
                                                     :size 20 :color +darkgreen+)))))
          (gameplay (make-scene () ((paint (make-screen +purple+))
                                    (text (make-text "GAMEPLAY SCREEN" 20 20
                                                     :size 40 :color +maroon+))
                                    (subtext (make-text "PRESS ENTER or TAP to JUMP to ENDING SCREEN"
                                                        130 220
                                                        :size 20 :color +maroon+)))))
          (ending (make-scene () ((paint (make-screen +blue+))
                                  (text (make-text "ENDING SCREEN" 20 20
                                                   :size 40 :color +darkblue+))
                                  (subtext (make-text "PRESS ENTER or TAP to RETURN to TITLE SCREEN"
                                                      120 220
                                                      :size 20 :color +darkblue+))))))
      ;; TODO: This nesting was a quick fix. Not the ideal way of doing scenes!
      (with-scene logo ()
        (with-scene title ()
          (with-scene gameplay ()
            (with-scene ending ()
              (do-game-loop (:livesupport t
                             :vars ((screens `(:logo ,@(alexandria:circular-list :title :gameplay :ending)))
                                    (frame-count 0)))
                (incf frame-count)
                (when (or (and (next-screen) (not (eql (car screens) :logo)))
                          (and (eql (car screens) :logo) (> frame-count (* 2 *target-fps*))))
                  (setf screens (cdr screens)))
                (with-drawing
                  (case (car screens)
                    (:logo (draw-scene-all logo))
                    (:title (draw-scene-all title))
                    (:gameplay (draw-scene-all gameplay))
                    (:ending (draw-scene-all ending))))))))))))
