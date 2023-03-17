(in-package #:claylib/examples)
(defpackage claylib/examples/multichannel-sound
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/multichannel-sound)

(defparameter *scene*
  (make-scene-pro ((:assets (fx-wav-file (make-sound-asset
                                          (claylib/examples:claylib-path
                                           "examples/audio/resources/sound.wav")))
                            (fx-ogg-file (make-sound-asset
                                          (claylib/examples:claylib-path
                                           "examples/audio/resources/target.ogg"))))
                   (:params (fx-wav (asset fx-wav-file))
                            (fx-ogg (asset fx-ogg-file)))
                   (:objects (header (make-text "MULTICHANNEL SOUND PLAYING"
                                                20 20
                                                :size 20
                                                :color +gray+))
                             (sub1 (make-text "Press SPACE to play new ogg instance!"
                                              200 120
                                              :size 20
                                              :color +lightgray+))
                             (sub2 (make-text "Press ENTER to play new wav instance!"
                                              200 180
                                              :size 20
                                              :color +lightgray+))
                             (playing (make-text "CONCURRENT SOUNDS PLAYING: 0"
                                                 220 280
                                                 :size 20
                                                 :color +red+))))))

(defun main ()
  (with-window (:title "raylib [audio] example - Multichannel sound playing")
    (with-scenes *scene* ()
      (with-scene-bindings () *scene*
        (setf (volume fx-wav) 0.2)
        (do-game-loop (:livesupport t)
          (when (is-key-pressed-p +key-enter+) (play fx-wav))
          (when (is-key-pressed-p +key-space+) (play fx-ogg))
          (setf (text playing)
                (format nil "CONCURRENT SOUNDS PLAYING: ~d" (get-sounds-playing)))
          (with-drawing ()
            (draw-scene-all *scene*)))))))
