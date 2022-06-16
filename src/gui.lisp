(in-package #:claylib)

;;;; Raygui Lispification
;;;; Obligatory disclaimer: Everything in this file is in flux and will probably get moved.
;;;; I don't fully know what direction this will go yet.



(defun-pt-bool gui-window-box claylib/ll:gui-window-box
  "Create a Raygui window box."
  (bounds rl-rectangle)
  (title string))
