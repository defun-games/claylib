(in-package #:cl-user)
(defpackage claylib/examples/window-letterbox
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/window-letterbox)

(defun clamp-vector2 (vec min max &optional allocate-p)
  (let ((vec (if allocate-p (make-vector2 (x vec) (y vec)) vec)))
    (setf (x vec) (alexandria:clamp (x vec) (x min) (x max))
          (y vec) (alexandria:clamp (y vec) (y min) (y max)))
    vec))

(defun make-colored-rect (width height i)
  (make-rectangle 0 (* i (/ height 10.0))
                  width (/ height 10.0)
                  (make-color (get-random-value 100 250)
                              (get-random-value 50 150)
                              (get-random-value 10 100))))

(defun main ()
  (with-window (:title "raylib [core] example - window scale letterbox"
                :flags `(,+flag-window-resizable+ ,+flag-vsync-hint+)
                :min-size (320 240))
    (let* ((game-screen-width 640)
           (game-screen-height 480)
           (target (load-render-texture game-screen-width game-screen-height))
           (virtual-mouse (make-vector2 0 0))
           (v2min (make-vector2 0 0))
           (v2max (make-vector2 game-screen-width game-screen-height))
           (scene (make-scene ()
                              ((text1 (make-text "If executed inside a window,
you can resize the window,
and see the screen scaling!"
                                                 10 25
                                                 :size 20 :color +white+))
                               (text2 (make-text "" 350 25 :size 20 :color +green+))
                               (text3 (make-text "" 350 55 :size 20 :color +yellow+))
                               (rect0 (make-colored-rect game-screen-width game-screen-height 0))
                               (rect1 (make-colored-rect game-screen-width game-screen-height 1))
                               (rect2 (make-colored-rect game-screen-width game-screen-height 2))
                               (rect3 (make-colored-rect game-screen-width game-screen-height 3))
                               (rect4 (make-colored-rect game-screen-width game-screen-height 4))
                               (rect5 (make-colored-rect game-screen-width game-screen-height 5))
                               (rect6 (make-colored-rect game-screen-width game-screen-height 6))
                               (rect7 (make-colored-rect game-screen-width game-screen-height 7))
                               (rect8 (make-colored-rect game-screen-width game-screen-height 8))
                               (rect9 (make-colored-rect game-screen-width game-screen-height 9))))))
      (setf (filter (texture target)) +texture-filter-bilinear+
            (source (texture target))
            (make-instance 'rl-rectangle
                           :x 0 :y 0
                           :width (width (texture target))
                           :height (- (height (texture target))))
            (dest (texture target))
            (make-instance 'rl-rectangle
                           :x (* 0.5
                                 (- (get-screen-width) game-screen-width))
                           :y (* 0.5
                                 (- (get-screen-height) game-screen-height))
                           :width game-screen-width
                           :height game-screen-height)
            (origin (texture target)) (make-vector2 0 0)
            (rot (texture target)) 0.0
            (tint (texture target)) +white+)
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (let ((scale (coerce (min (/ (get-screen-width) game-screen-width)
                                    (/ (get-screen-height) game-screen-height))
                               'float))
                (mouse-x (get-mouse-x))
                (mouse-y (get-mouse-y)))
            (when (is-key-pressed-p +key-space+)
              (dolist (rect '(rect0 rect1 rect2 rect3 rect4 rect5 rect6 rect7 rect8 rect9))
                (let ((color (color (scene-object scene rect))))
                  (setf (r color) (get-random-value 100 250)
                        (g color) (get-random-value 50 150)
                        (b color) (get-random-value 10 100)))))
            (setf (x virtual-mouse) (/ (- mouse-x
                                          (* 0.5
                                             (- (get-screen-width)
                                                (* game-screen-width scale))))
                                       scale)
                  (y virtual-mouse) (/ (- mouse-y
                                          (* 0.5
                                             (- (get-screen-height)
                                                (* game-screen-height scale))))
                                       scale))
            (clamp-vector2 virtual-mouse v2min v2max)

            ;; Apply the same transformation as the virtual mouse to the real mouse (i.e. to work with raygui)
            #|
            (set-mouse-offset (truncate (* -0.5
                                           (- (get-screen-width) (* game-screen-width scale))))
                              (truncate (* -0.5
                                           (- (get-screen-height) (* game-screen-height scale)))))
            (set-mouse-scale (/ 1 scale) (/ 1 scale))
            |#

            (setf (text (scene-object scene 'text2))
                  (format nil "Default Mouse: [~d , ~d]" mouse-x mouse-y)
                  (text (scene-object scene 'text3))
                  (format nil "Virtual Mouse: [~d , ~d]" (x virtual-mouse) (y virtual-mouse)))
            (let ((dest (dest (texture target))))
              (setf (x dest) (* 0.5
                                (- (get-screen-width) (* game-screen-width scale)))
                    (y dest) (* 0.5
                                (- (get-screen-height) (* game-screen-height scale)))
                    (width dest) (* game-screen-width scale)
                    (height dest) (* game-screen-height scale)))
            (with-texture-mode (target)
              (draw-scene-regex scene "^RECT")
              (draw-scene-regex scene "^TEXT"))
            (with-drawing (:bgcolor +black+)
              (draw-object (texture target)))))))))
