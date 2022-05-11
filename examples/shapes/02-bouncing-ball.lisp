(in-package #:claylib/examples)

(defclass ball (circle)
  ((%speed :initarg :speed
           :type rl-vector2
           :reader speed)))

(defun make-ball (x y radius color vx vy &key (filled t))
  "Make an BALL instance where VX and XY are the x and y components of the BALL's SPEED."
  (make-instance 'ball
                 :pos (make-vector2 x y)
                 :radius radius
                 :color color
                 :speed (make-vector2 vx vy)
                 :filled filled))

(defun move-ball (ball)
  "Update BALL position based on its speed."
  (incf (x ball) (x (speed ball)))
  (incf (y ball) (y (speed ball))))

(defun bounce-ball (ball screen-width screen-height)
  "Negate the speed of the given BALL to make it bounce off the edges of the screen."
  (let ((x (x ball))
        (y (y ball))
        (radius (radius ball)))
    (when (or (>= x (- screen-width radius)) (<= x radius))
      (setf (x (speed ball)) (- (x (speed ball)))))
    (when (or (>= y (- screen-height radius)) (<= y radius))
      (setf (y (speed ball)) (- (y (speed ball)))))))

(defun example-shapes-02 ()
  (with-window (:title "raylib [shapes] example - bouncing ball")
    (let ((scene (make-scene ()
                             `((ball ,(make-ball (/ (get-screen-width) 2.0)
                                                 (/ (get-screen-height) 2.0)
                                                 20
                                                 +maroon+
                                                 5
                                                 4))
                               (text ,(make-text "PRESS SPACE to PAUSE BALL MOVEMENT"
                                                 10
                                                 (- (get-screen-height) 25)
                                                 :size 20
                                                 :color +lightgray+))
                               (text-pause ,(make-text "PAUSED"
                                                       350
                                                       200
                                                       :size 30
                                                       :color +gray+))))))
      (with-scene scene ()
        (with-scene-objects (ball) scene
          (do-game-loop (:livesupport t
                         :vars ((pause nil)
                                (frames-counter 0)))
            (when (is-key-pressed-p +key-space+)
              (setf pause (not pause)))

            (if (not pause)
                (progn
                  (move-ball ball)
                  (bounce-ball ball (get-screen-width) (get-screen-height)))
                (incf frames-counter))

            (with-drawing
              (if (and pause (= 0 (mod (truncate frames-counter 30) 2)))
                  (draw-scene-all scene)
                  (draw-scene-except scene 'text-pause))
              (draw-fps 10 10))))))))
