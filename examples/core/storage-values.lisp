(in-package #:cl-user)
(defpackage claylib/examples/storage-values
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/storage-values)

(defun save-storage-value (position value &optional (filename "storage.data"))
  "Saves the 32 bit integer VALUE at POSITION (index) in the file named FILENAME."
  (with-open-file (f filename :direction :output
                              :element-type '(unsigned-byte 32)
                              :if-exists :overwrite
                              :if-does-not-exist :create)
    (file-position f position)
    (write-byte value f)))

(defun load-storage-value (position &optional (filename "storage.data"))
  (with-open-file (f filename :element-type '(unsigned-byte 32)
                              :if-does-not-exist :create)
    (file-position f position)
    (read-byte f nil 0)))

(defun main ()
  (with-window (:title "raylib [core] example - storage save/load values")
    (let ((score 0)
          (hiscore 0)
          (frames-counter 0)
          (storage-file (claylib/examples:claylib-path "examples/core/storage.data"))
          (scene (make-scene ()
                             ((text1 (make-text "SCORE: 0" 280 130 :size 40 :color +maroon+))
                              (text2 (make-text "HI-SCORE: 0" 210 200 :size 50 :color +black+))
                              (text3 (make-text "frames: 0" 10 10 :size 20 :color +lime+))
                              (text4 (make-text "Press R to generate random numbers"
                                                220 40
                                                :size 20 :color +lightgray+))
                              (text5 (make-text "Press ENTER to SAVE values"
                                                250 310
                                                :size 20 :color +lightgray+))
                              (text6 (make-text "Press SPACE to LOAD values"
                                                252 350
                                                :size 20 :color +lightgray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (when (is-key-pressed-p +key-r+)
            (setf score (get-random-value 1000 2000)
                  hiscore (get-random-value 2000 4000)))
          (cond
            ((is-key-pressed-p +key-enter+)
             (save-storage-value 0 score storage-file) (save-storage-value 1 hiscore storage-file))
            ((is-key-pressed-p +key-space+)
             (setf score (load-storage-value 0 storage-file)
                   hiscore (load-storage-value 1 storage-file))))
          (incf frames-counter)
          (setf (text (scene-object scene 'text1)) (format nil "SCORE: ~d" score)
                (text (scene-object scene 'text2)) (format nil "HI-SCORE: ~d" hiscore)
                (text (scene-object scene 'text3)) (format nil "frames: ~d" frames-counter))
          (with-drawing ()
            (draw-scene-all scene)))))))
