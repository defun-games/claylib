(in-package #:cl-user)
(defpackage claylib/examples/window-flags
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/window-flags)

(defclass ball (circle)
  ((%speed :initarg :speed
           :type rl-vector2
           :accessor spd)))

(declaim (inline toggle-flag))
(defun toggle-flag (key flag)
  (when (is-key-pressed-p key)
    (if (is-window-state-p flag)
        (clear-window-state flag)
        (set-window-state flag))))

(declaim (inline adjust-text))
(defun adjust-text (flag obj text)
  (if (is-window-state-p flag)
      (setf (text obj) (format nil "~A: on" text)
            (color obj) +lime+)
      (setf (text obj) (format nil "~A: off" text)
            (color obj) +maroon+)))

(defun main ()
  (with-window (:title "raylib [core] example - window flags")
    (let ((scene (make-scene ()
                             ((ball (make-instance 'ball
                                                   :pos (make-vector2 (/ (get-screen-width) 2.0)
                                                                      (/ (get-screen-height) 2.0))
                                                   :radius 20
                                                   :color +maroon+
                                                   :speed (make-vector2 5 4)))
                              (mouse (make-circle 0 0 10 +darkblue+))
                              (rect (make-rectangle 0 0
                                                    (get-screen-width) (get-screen-height)
                                                    +raywhite+ :filled nil :thickness 4))
                              (header (make-text "" 10 40 :size 10 :color +green+))
                              (subheader1 (make-text "Following flags can be set after window creation:"
                                                     10 60
                                                     :size 10 :color +gray+))
                              (subheader2 (make-text "Following flags can only be set before window creation:"
                                                     10 300
                                                     :size 10 :color +gray+))
                              (flag-fullscreen (make-text "" 10 80 :size 10))
                              (flag-resizable (make-text "" 10 100 :size 10))
                              (flag-undecorated (make-text "" 10 120 :size 10))
                              (flag-hidden (make-text "" 10 140 :size 10))
                              (flag-minimized (make-text "" 10 160 :size 10))
                              (flag-maximized (make-text "" 10 180 :size 10))
                              (flag-unfocused (make-text "" 10 200 :size 10))
                              (flag-topmost (make-text "" 10 220 :size 10))
                              (flag-always-run (make-text "" 10 240 :size 10))
                              (flag-vsync-hint (make-text "" 10 260 :size 10))
                              (flag-highdpi (make-text "" 10 320 :size 10))
                              (flag-transparent (make-text "" 10 340 :size 10))
                              (flag-msaa-4x-hint (make-text "" 10 360 :size 10))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t
                       :vars ((frames-counter 0)))
          (when (is-key-pressed-p +key-f+)
            (toggle-fullscreen))
          (toggle-flag +key-r+ +flag-window-resizable+)
          (toggle-flag +key-d+ +flag-window-undecorated+)

          (when (is-key-pressed-p +key-h+)
            (unless (is-window-state-p +flag-window-hidden+)
              (set-window-state +flag-window-hidden+))
            (setf frames-counter 0))
          (when (is-window-state-p +flag-window-hidden+)
            (incf frames-counter)
            (when (>= frames-counter 240) (clear-window-state +flag-window-hidden+)))

          (when (is-key-pressed-p +key-n+)
            (unless (is-window-state-p +flag-window-minimized+) (minimize-window))
            (setf frames-counter 0))
          (when (is-window-state-p +flag-window-minimized+)
            (incf frames-counter)
            (when (>= frames-counter 240) (restore-window)))

          (when (is-key-pressed-p +key-m+)
            (if (is-window-state-p +flag-window-maximized+)
                (restore-window)
                (maximize-window)))

          (toggle-flag +key-u+ +flag-window-unfocused+)
          (toggle-flag +key-t+ +flag-window-topmost+)
          (toggle-flag +key-a+ +flag-window-always-run+)
          (toggle-flag +key-v+ +flag-vsync-hint+)

          (let ((ball (scene-object scene 'ball)))
            (incf (x ball) (x (spd ball)))
            (incf (y ball) (y (spd ball)))

            (when (or (>= (x ball) (- (get-screen-width) (radius ball)))
                      (<= (x ball) (radius ball)))
              (setf (x (spd ball)) (- (x (spd ball)))))
            (when (or (>= (y ball) (- (get-screen-height) (radius ball)))
                      (<= (y ball) (radius ball)))
              (setf (y (spd ball)) (- (y (spd ball))))))

          (with-drawing (:bgcolor (if (is-window-state-p +flag-window-transparent+)
                                      +blank+
                                      +raywhite+))
            (setf (width (scene-object scene 'rect)) (get-screen-width)
                  (height (scene-object scene 'rect)) (get-screen-height)
                  (text (scene-object scene 'header))
                  (format nil "Screen size: [~d, ~d]" (get-screen-width) (get-screen-height)))
            (get-mouse-position :vec (pos (scene-object scene 'mouse)))
            (adjust-text +flag-fullscreen-mode+
                         (scene-object scene 'flag-fullscreen)
                         "[F] FLAG_FULLSCREEN_MODE")
            (adjust-text +flag-window-resizable+
                         (scene-object scene 'flag-resizable)
                         "[R] FLAG_WINDOW_RESIZABLE")
            (adjust-text +flag-window-undecorated+
                         (scene-object scene 'flag-undecorated)
                         "[D] FLAG_WINDOW_UNDECORATED")
            (adjust-text +flag-window-hidden+
                         (scene-object scene 'flag-hidden)
                         "[H] FLAG_WINDOW_HIDDEN")
            (adjust-text +flag-window-minimized+
                         (scene-object scene 'flag-minimized)
                         "[N] FLAG_WINDOW_MINIMIZED")
            (adjust-text +flag-window-maximized+
                         (scene-object scene 'flag-maximized)
                         "[M] FLAG_WINDOW_MAXIMIZED")
            (adjust-text +flag-window-unfocused+
                         (scene-object scene 'flag-unfocused)
                         "[G] FLAG_WINDOW_UNFOCUSED")
            (adjust-text +flag-window-topmost+
                         (scene-object scene 'flag-topmost)
                         "[T] FLAG_WINDOW_TOPMOST")
            (adjust-text +flag-window-always-run+
                         (scene-object scene 'flag-always-run)
                         "[A] FLAG_WINDOW_ALWAYS_RUN")
            (adjust-text +flag-vsync-hint+
                         (scene-object scene 'flag-vsync-hint)
                         "[V] FLAG_VSYNC_HINT")
            (adjust-text +flag-window-highdpi+
                         (scene-object scene 'flag-highdpi)
                         "FLAG_WINDOW_HIGHDPI")
            (adjust-text +flag-window-transparent+
                         (scene-object scene 'flag-transparent)
                         "FLAG_WINDOW_TRANSPARENT")
            (adjust-text +flag-msaa-4x-hint+
                         (scene-object scene 'flag-msaa-4x-hint)
                         "FLAG_MSAA_4X_HINT")

            (draw-scene scene '(ball rect mouse header subheader1 subheader2))
            (draw-fps 10 10)
            (draw-scene-regex scene "^FLAG-")))))))
