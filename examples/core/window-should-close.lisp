(in-package #:cl-user)
(defpackage claylib/examples/window-should-close
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/window-should-close)

(defparameter *scene*
  (make-scene ()
              ((text-box (make-rectangle 0 100 (get-screen-width) 200 +black+))
               (text-confirmation (make-text "Are you sure you want to exit program? [Y/N]"
                                             40 180
                                             :size 30
                                             :color +white+))
               (text (make-text "Try to close the window to get confirmation message!"
                                120 200
                                :size 20
                                :color +lightgray+)))))

(defun main ()
  (with-window (:title "raylib [core] example - window should close"
                :exit-key +key-null+)
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t
                     :vars ((exit-window-requested nil)
                            (exit-window nil))
                     :end exit-window)
        (when (or (window-should-close-p) (is-key-pressed-p +key-escape+))
          (setf exit-window-requested t))

        (when exit-window-requested
          (if (is-key-pressed-p +key-y+)
              (setf exit-window t)
              (when (is-key-pressed-p +key-n+)
                (setf exit-window-requested nil))))

        (with-drawing ()
          (if exit-window-requested
              (draw-scene *scene* '(text-box text-confirmation))
              (draw-scene *scene* 'text)))))))
