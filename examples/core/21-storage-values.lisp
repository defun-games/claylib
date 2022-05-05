(in-package #:claylib/examples)

(defun example-core-21 ()
  (with-window (:title "raylib [core] example - storage save/load values")
    (let ((score 0)
          (hiscore 0)
          (frames-counter 0)
          (scene (make-scene ()
                             `((text1 ,(make-text "SCORE: 0" 280 130 :size 40 :color +maroon+))
                               (text2 ,(make-text "HI-SCORE: 0" 210 200 :size 50 :color +black+))
                               (text3 ,(make-text "frames: 0" 10 10 :size 20 :color +lime+))
                               (text4 ,(make-text "Press R to generate random numbers"
                                                  220 40
                                                  :size 20 :color +lightgray+))
                               (text5 ,(make-text "Press ENTER to SAVE values"
                                                  250 310
                                                  :size 20 :color +lightgray+))
                               (text6 ,(make-text "Press SPACE to LOAD values"
                                                  252 350
                                                  :size 20 :color +lightgray+))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t)
          (when (is-key-pressed-p +key-r+)
            (setf score (get-random-value 1000 2000)
                  hiscore (get-random-value 2000 4000)))
          (cond
            ((is-key-pressed-p +key-enter+)
             (save-storage-value 0 score) (save-storage-value 1 hiscore))
            ((is-key-pressed-p +key-space+)
             (setf score (load-storage-value 0)
                   hiscore (load-storage-value 1))))
          (incf frames-counter)
          (setf (text (scene-object scene 'text1)) (format nil "SCORE: ~d" score)
                (text (scene-object scene 'text2)) (format nil "HI-SCORE: ~d" hiscore)
                (text (scene-object scene 'text3)) (format nil "frames: ~d" frames-counter))
          (with-drawing (draw-scene-all scene)))))))
