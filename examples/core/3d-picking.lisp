(in-package #:cl-user)
(defpackage claylib/examples/3d-picking
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/3d-picking)

(defun main ()
  (with-window (:title "raylib [core] example - 3d picking")
    (let ((camera (make-camera-3d 10 10 10
                                  0 0 0
                                  0 1 0
                                  :fovy 45.0
                                  :projection +camera-perspective+
                                  :mode +camera-first-person+))
          (scene (make-scene ()
                             ((cube (make-cube 0 1 0
                                               2 2 2
                                               +gray+))
                              (outline (make-cube 0 1 0
                                                  2 2 2
                                                  +darkgray+
                                                  :filled nil))
                              (indicator (make-cube 0 1 0
                                                    2.2 2.2 2.2
                                                    +green+
                                                    :filled nil))
                              (ray (make-ray 0 0 0 0 0 0 +maroon+))
                              (grid (make-grid 10 1.0))
                              (text1 (make-text "Try clicking on the box with your mouse!"
                                                240 10
                                                :size 20 :color +darkgray+))
                              (text2 (make-text "BOX SELECTED"
                                                (/ (- *screen-width*
                                                      (measure-text "BOX SELECTED" 30))
                                                   2.0)
                                                (* *screen-height* 0.1)
                                                :size 30
                                                :color +green+))
                              (text3 (make-text "Right click mouse to toggle camera controls"
                                                10 430
                                                :size 10 :color +gray+)))))
          (collision (make-ray-collision 0 0 0 0 0 0))
          (mouse-pos (make-vector2 0 0))
          (bbox (make-instance 'rl-bounding-box :low (make-vector3 0 0 0)
                                                :high (make-vector3 0 0 0))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (when (is-cursor-hidden-p)
            (update-camera camera))
          (when (is-mouse-button-pressed-p +mouse-button-right+)
            (if (is-cursor-hidden-p) (enable-cursor) (disable-cursor)))
          (when (is-mouse-button-pressed-p +mouse-button-left+)
            (if (hit collision)
                (setf (hit collision) nil)
                (let* ((cube (scene-object scene 'cube))
                       (ray (scene-object scene 'ray))
                       (half-w (/ (width cube) 2.0))
                       (half-h (/ (height cube) 2.0))
                       (half-l (/ (len cube) 2.0)))
                  (setf (x (low bbox)) (- (x cube) half-w)
                        (y (low bbox)) (- (y cube) half-h)
                        (z (low bbox)) (- (z cube) half-l)
                        (x (high bbox)) (+ (x cube) half-w)
                        (y (high bbox)) (+ (y cube) half-h)
                        (z (high bbox)) (+ (z cube) half-l))
                  (get-mouse-ray (get-mouse-position :vec mouse-pos) camera :ray ray)
                  (get-ray-collision-box ray bbox :rc collision))))
          (with-drawing ()
            (with-3d-mode camera
              (if (hit collision)
                  (progn
                    (setf (color (scene-object scene 'cube)) +red+
                          (color (scene-object scene 'outline)) +maroon+)
                    (draw-scene scene '(cube outline indicator ray grid)))
                  (progn
                    (setf (color (scene-object scene 'cube)) +gray+
                          (color (scene-object scene 'outline)) +darkgray+)
                    (draw-scene scene '(cube outline ray grid)))))
            (draw-scene scene '(text1 text3))
            (when (hit collision) (draw-scene scene 'text2))
            (draw-fps 10 10)))))))
