(in-package #:claylib/examples)

(defun example-core-01 ()
  (with-window (:title "raylib [core] example - basic window")
    (let ((scene (make-scene ()
                             `((text ,(make-text "Congrats! You created your first window!"
                                                 190
                                                 200
                                                 :size 20
                                                 :color +lightgray+))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t)
          (with-drawing
            (draw-scene-all scene)))))))

