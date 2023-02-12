(in-package #:cl-user)
(defpackage claylib/examples/box-collisions
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/box-collisions)

(defun cube-bbox (cube &key (bbox (make-instance 'rl-bounding-box
                                                 :low (make-vector3 0 0 0)
                                                 :high (make-vector3 0 0 0))))
  "Return a bounding box matching a CUBE. Alloctaes a new RL-BOUNDING-BOX unless BBOX is given."
  (setf (x (low bbox)) (- (x cube) (/ (width cube) 2))
        (y (low bbox)) (- (y cube) (/ (height cube) 2))
        (z (low bbox)) (- (z cube) (/ (len cube) 2))
        (x (high bbox)) (+ (x cube) (/ (width cube) 2))
        (y (high bbox)) (+ (y cube) (/ (height cube) 2))
        (z (high bbox)) (+ (z cube) (/ (len cube) 2)))
  bbox)

(defparameter *scene*
  (make-scene ()
              ((camera (make-camera-3d 0 10 10
                                       0 0 0
                                       0 1 0))
               (player (make-cube 0 1 2
                                  1 2 1
                                  +green+))
               (pbbox (make-instance 'rl-bounding-box
                                     :low (make-vector3 0 0 0)
                                     :high (make-vector3 0 0 0)))
               (enemy-box (make-cube -4 1 0
                                     2 2 2
                                     +gray+))
               (box-wires (make-cube -4 1 0
                                     2 2 2
                                     +darkgray+
                                     :filled nil))
               (ebbox (make-instance 'rl-bounding-box
                                     :low (make-vector3 0 0 0)
                                     :high (make-vector3 0 0 0)))
               (enemy-sphere (make-sphere 4 0 0
                                          1.5
                                          +gray+))
               (sphere-wires (make-sphere 4 0 0
                                          1.5
                                          +darkgray+
                                          :filled nil
                                          :rings 16
                                          :slices 16))
               (grid (make-grid 10 1))
               (text (make-text "Move player with cursors to collide"
                                220 40 :size 20 :color +gray+)))))

(defun main ()
  (with-window (:title "raylib [models] example - box collisions")
    (with-scenes *scene* ()
      (with-scene-objects (camera player enemy-box enemy-sphere pbbox ebbox box-wires sphere-wires grid) *scene*
        (cube-bbox enemy-box :bbox ebbox)
        (do-game-loop (:livesupport t
                       :vars ((collision nil)))
          ;; Move player
          (cond ((is-key-down-p +key-right+) (incf (x player) 0.2))
                ((is-key-down-p +key-left+)  (decf (x player) 0.2))
                ((is-key-down-p +key-down+)  (incf (z player) 0.2))
                ((is-key-down-p +key-up+)    (decf (z player) 0.2)))

          ;; Collisions
          (cube-bbox player :bbox pbbox)
          (setf collision (or (check-collision-boxes pbbox ebbox)
                              (check-collision-box-sphere pbbox enemy-sphere))
                (color player) (if collision +red+ +green+))

          (with-drawing ()
            (with-3d-mode camera
              (draw-objects (list player enemy-box enemy-sphere box-wires sphere-wires grid)))
            (draw-scene *scene* 'text)
            (draw-fps 10 10)))))))
