(in-package #:cl-user)
(defpackage claylib/examples/audio-2
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/audio-2)

(defparameter *scene*
  (make-scene ()
              ((title (make-text "MUSIC SHOULD BE PLAYING!" 255 150 :size 20 :color +lightgray+))
               (timebar-bg (make-rectangle 200 200 400 12 +lightgray+))
               (timebar-fill (make-rectangle 200 200 0 12 +maroon+))
               (timebar-line (make-rectangle 200 200 400 12 +gray+ :filled nil))
               (text1 (make-text "PRESS SPACE TO RESTART MUSIC"
                                 215 250
                                 :size 20
                                 :color +lightgray+))
               (text2 (make-text "PRESS P TO PAUSE/RESUME MUSIC"
                                 208 280
                                 :size 20
                                 :color +lightgray+)))))

(defun main ()
  (with-window (:title "raylib [audio] example - music playing (streaming)"
                :fps 30)
    (with-audio-device
      (with-music-stream music (claylib/examples:claylib-path "examples/audio/resources/country.mp3")
        (play-music-stream music)
        (with-scenes *scene*
          (do-game-loop (:livesupport t
                         :vars ((time-played 0)
                                (pause nil)))
            (update-music-stream music)

            (when (is-key-pressed-p +key-space+)
              (stop-music-stream music)
              (play-music-stream music))

            (when (is-key-pressed-p +key-p+)
              (if (setf pause (not pause))
                  (pause-music-stream music)
                  (resume-music-stream music)))

            (setf time-played (min (/ (get-music-time-played music)
                                      (get-music-time-length music))
                                   1)
                  (width (scene-object *scene* 'timebar-fill)) (* time-played 400))

            (with-drawing ()
              (draw-scene-all *scene*))))))))
