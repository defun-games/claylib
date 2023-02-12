(in-package #:cl-user)
(defpackage claylib/examples/smooth-pixelperfect
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/smooth-pixelperfect)

(defun main ()
  (with-window (:title "raylib [core] example - smooth pixel-perfect camera")
    (let* ((virtual-screen-width 160)
           (virtual-screen-height 90)
           (virtual-ratio (float (/ (get-screen-width) virtual-screen-width)))
           (world-space-camera (make-camera-2d 0 0 0 0))
           (screen-space-camera (make-camera-2d 0 0 0 0))
           (target (load-render-texture virtual-screen-width virtual-screen-height))
           (origin (make-vector2 0 0))
           (scene (make-scene ()
                              ((rec1 (make-rectangle 70 35 20 20 +black+
                                                     :origin origin))
                               (rec2 (make-rectangle 90 55 30 10 +red+
                                                     :origin origin))
                               (rec3 (make-rectangle 80 65 15 25 +blue+
                                                     :origin origin))
                               (tex (texture target))
                               (text1 (make-text (format nil "Screen resolution: ~dx~d"
                                                         (get-screen-width)
                                                         (get-screen-height))
                                                 10 10
                                                 :size 20 :color +darkblue+))
                                (text2 (make-text (format nil "World resolution: ~dx~d"
                                                          virtual-screen-width
                                                          virtual-screen-height)
                                                  10 40
                                                  :size 20 :color +darkgreen+))))))

      (with-scenes scene ()
        (let ((tex (scene-object scene 'tex)))
          (setf (source tex) (make-simple-rec 0 0 (width tex) (- (height tex)))
                (dest tex) (make-simple-rec (- virtual-ratio)
                                            (- virtual-ratio)
                                            (+ (get-screen-width) (* virtual-ratio 2))
                                            (+ (get-screen-height) (* virtual-ratio 2)))
                (tint tex) +white+))
        (do-game-loop (:livesupport t
                       :vars ((rotation 0.0)))
          (incf rotation (* 60.0 (get-frame-time)))
          (let ((camera-x (coerce (- (* (sin (get-time))
                                        50.0)
                                     10.0)
                                  'single-float))
                (camera-y (coerce (* (cos (get-time))
                                     30.0)
                                  'single-float))
                (starget (target screen-space-camera))
                (wtarget (target world-space-camera)))
            (setf (x starget) camera-x
                  (y starget) camera-y
                  (x wtarget) (truncate camera-x)
                  (y wtarget) (truncate camera-y))
            (decf (x starget) (x wtarget))
            (decf (y starget) (y wtarget))
            (setf (x starget) (* (x starget) virtual-ratio)
                  (y starget) (* (y starget) virtual-ratio)))

          (setf (rot (scene-object scene 'rec1)) rotation
                (rot (scene-object scene 'rec2)) (- rotation)
                (rot (scene-object scene 'rec3)) (+ rotation 45.0))

          (with-texture-mode (target)
            (with-2d-mode world-space-camera
              (draw-scene-regex scene "^REC[0-9]")))

          (with-drawing (:bgcolor +red+)
            (with-2d-mode screen-space-camera
              (draw-scene scene 'tex))
            (draw-scene scene '(text1 text2))
            (draw-fps (- (get-screen-width) 95) 10)))))))
