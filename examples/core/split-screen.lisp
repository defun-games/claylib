(in-package #:cl-user)
(defpackage claylib/examples/split-screen
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/split-screen)

(defun main ()
  (with-window (:title "raylib [core] example - split screen")
    (let* ((camera-player1 (make-camera-3d 0 1 -3
                                           0 1 0
                                           0 1 0))
           (camera-player2 (make-camera-3d -3 3 0
                                           0 3 0
                                           0 1 0))
           (screen-player1 (load-render-texture (/ (get-screen-width) 2)
                                                (get-screen-height)))
           (screen-player2 (load-render-texture (/ (get-screen-width) 2)
                                                (get-screen-height)))
           (split-screen-rect (make-simple-rec 0 0
                                               (/ (get-screen-width) 2)
                                               (- (get-screen-height))))
           (scene (make-scene ()
                              ((ground (make-plane 0 0 0 50 50 +beige+))
                               (player1 (make-cube 0 1 -3
                                                   1 1 1
                                                   +red+))
                               (player2 (make-cube -3 -3 0
                                                   1 1 1
                                                   +blue+))
                               (text-player1 (make-text "PLAYER1 W/S to move"
                                                        10 10
                                                        :size 20 :color +red+))
                               (text-player2 (make-text "PLAYER2 UP/DOWN to move"
                                                        10 10
                                                        :size 20 :color +blue+))
                               (texture-player1 (texture screen-player1))
                               (texture-player2 (texture screen-player2))))))
      (loop for x from -20 to 20 by 4
            do (loop for z from -20 to 20 by 4
                     do (setf (gethash (gensym "TREE") (objects scene))
                              (make-cube x 1.5 z
                                         1 1 1
                                         +lime+)
                              (gethash (gensym "TREE") (objects scene))
                              (make-cube x 0.5 z
                                         0.25 1 0.25
                                         +brown+))))
      (with-scenes scene ()
        (let ((tex1 (scene-object scene 'texture-player1))
              (tex2 (scene-object scene 'texture-player2)))
          (setf (source tex1) split-screen-rect
                (source tex2) split-screen-rect
                (dest tex1) (make-instance 'rl-rectangle
                                           :x 0 :y 0
                                           :width (width tex1) :height (height tex1))
                (dest tex2) (make-instance 'rl-rectangle
                                           :x (/ (get-screen-width) 2.0) :y 0
                                           :width (width tex2) :height (height tex2))
                (tint tex1) +white+
                (tint tex2) +white+))
        (do-game-loop (:livesupport t)
          (let ((offset-this-frame (* 10.0 (get-frame-time))))
            (when (is-key-down-p +key-w+)
              (incf (z camera-player1) offset-this-frame)
              (incf (z (target camera-player1)) offset-this-frame))
            (when (is-key-down-p +key-s+)
              (decf (z camera-player1) offset-this-frame)
              (decf (z (target camera-player1)) offset-this-frame))

            (when (is-key-down-p +key-up+)
              (incf (x camera-player2) offset-this-frame)
              (incf (x (target camera-player2)) offset-this-frame))
            (when (is-key-down-p +key-down+)
              (decf (x camera-player2) offset-this-frame)
              (decf (x (target camera-player2)) offset-this-frame)))

          (setf (pos (scene-object scene 'player1)) (pos camera-player1)
                (pos (scene-object scene 'player2)) (pos camera-player2))

          (with-texture-mode (screen-player1 :clear +skyblue+)
            (with-3d-mode camera-player1
              (draw-scene scene 'ground)
              (draw-scene-regex scene "^TREE")
              (draw-scene scene 'player2))
            (draw-scene scene 'text-player1))

          (with-texture-mode (screen-player2 :clear +skyblue+)
            (with-3d-mode camera-player2
              (draw-scene scene 'ground)
              (draw-scene-regex scene "^TREE")
              (draw-scene scene 'player1))
            (draw-scene scene 'text-player2))

          (with-drawing (:bgcolor +black+)
            (draw-scene scene '(texture-player1 texture-player2))))))))
