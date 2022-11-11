(in-package #:cl-user)
(defpackage claylib/examples/gui-portable-window
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/gui-portable-window)

(defparameter *scene*
  (make-scene () ((text (make-text "Mouse Position: [ 0.0, 0.0 ]"
                                   10 40
                                   :size 10
                                   :color +darkgray+)))))

(defun set-window-pos (vec)
  (set-window-position (truncate (x vec))
                       (truncate (y vec))))

(defun main ()
  (with-window (:width 800
                :height 600
                :title "raygui - portable window"
                :flags `(,+flag-window-undecorated+))
    (let* ((mouse-pos (make-vector2 0 0))
           (window-pos (make-vector2 500 200))
           (pan-offset (make-vector2 0 0))
           (drag-window nil)
           (exit-window nil)
           (exit-window-rec (make-instance 'rl-rectangle
                                           :x 0
                                           :y 0
                                           :width 800
                                           :height 600))
           (collision-rec (make-instance 'rl-rectangle
                                         :x 0
                                         :y 0
                                         :width 800
                                         :height 20)))
      (set-window-pos window-pos)
      (with-scenes *scene* ()
        (do-game-loop (:livesupport t
                       :end exit-window)
          (get-mouse-position :vec mouse-pos)
          (when (and (is-mouse-button-pressed-p +mouse-button-left+)
                     (check-collision-point-rec mouse-pos collision-rec))
            (setf drag-window t
                  (x pan-offset) (x mouse-pos)
                  (y pan-offset) (y mouse-pos)))
          (when drag-window
            (incf (x window-pos) (- (x mouse-pos) (x pan-offset)))
            (incf (y window-pos) (- (y mouse-pos) (y pan-offset)))
            (when (is-mouse-button-released-p +mouse-button-left+)
              (setf drag-window nil))
            (set-window-pos window-pos))
          (with-drawing ()
            (setf exit-window (gui-window-box exit-window-rec "#198# PORTABLE WINDOW")
                  (text (scene-object *scene* 'text)) (format nil
                                                              "Mouse Position: [ ~d, ~d ]"
                                                              (round (x mouse-pos))
                                                              (round (y mouse-pos))))
            (draw-scene-all *scene*)))))))
