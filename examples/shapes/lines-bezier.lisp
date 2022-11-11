(in-package #:cl-user)
(defpackage claylib/examples/lines-bezier
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/lines-bezier)

(defparameter *scene*
  (make-scene ()
              ((text (make-text "USE MOUSE LEFT-RIGHT CLICK to DEFINE LINE START and END POINTS"
                                15 20
                                :size 20))
               (line (make-line-2d 0 0 *screen-width* *screen-height*
                                   +red+
                                   :thickness 2
                                   :bezier t)))))

(defun main ()
  (with-window (:title "raylib [shapes] example - cubic-bezier lines"
                :flags (list +flag-msaa-4x-hint+))
    (with-scenes *scene* ()
      (with-scene-objects (line) *scene*
        (do-game-loop (:livesupport t)
          (if (is-mouse-button-down-p +mouse-button-left+)
              (setf (start line) (get-mouse-position))
              (when (is-mouse-button-down-p +mouse-button-right+)
                (setf (end line) (get-mouse-position))))

          (with-drawing ()
            (draw-scene-all *scene*)))))))
