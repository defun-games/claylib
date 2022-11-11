(in-package #:cl-user)
(defpackage claylib/examples/format-text
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/format-text)

(defparameter *scene*
  (make-scene ()
              ((info (list (make-text "" 200  80 :size 20 :color +red+)
                           (make-text "" 200 120 :size 20 :color +green+)
                           (make-text "" 200 160 :size 40 :color +blue+)
                           (make-text "" 200 220 :size 20 :color +black+))))))

(defun main ()
  (with-window (:title "raylib [text] example - text formatting")
    (with-scenes *scene* ()
      (do-game-loop (:livesupport t
                     :vars ((score 100020)
                            (hiscore 200450)
                            (lives 5)
                            (time (get-frame-time) (get-frame-time))))
        (loop for text in (scene-object *scene* 'info)
              for ctrl-str in '("Score: ~8,'0d"
                                "HiScore: ~8,'0d"
                                "Lives: ~2,'0d"
                                "Frame Time: ~2,2f ms")
              for var in (list score hiscore lives (* time 1000))
              do (setf (text text) (format nil ctrl-str var)))
        (with-drawing ()
          (draw-scene-all *scene*))))))
