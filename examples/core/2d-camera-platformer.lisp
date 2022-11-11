(in-package #:cl-user)
(defpackage claylib/examples/2d-camera-platformer
  (:use :cl :claylib)
  (:import-from :alexandria
                :circular-list)
  (:export :main))
(in-package #:claylib/examples/2d-camera-platformer)

(defclass player (rectangle)
  ((%speed :initarg :spd
           :type float
           :accessor spd)
   (%can-jump :initarg :can-jump-p
              :type boolean
              :accessor can-jump-p)))

(defclass env-item (rectangle)
  ((%blocking-p :initarg :blocking-p
                :type boolean
                :accessor blocking-p)))

(defclass camera (rl-camera-2d)
  ((%mode :initform (circular-list :center
                                              :center-inside-map
                                              :center-smooth-follow
                                              :even-out-on-landing
                                              :player-bounds-push))
   (%mode-desc :initform
               (circular-list "Follow player center"
                                         "Follow player center, but clamp to map edges"
                                         "Follow player center; smoothed"
                                         "Follow player center horizontally; update player center vertically after landing"
                                         "Player push camera on getting too close to screen edge"))
   (%even-out-speed :initform 700.0
                    :type float
                    :accessor even-out-speed)
   (%even-out-p :initform nil
                :type boolean
                :accessor even-out-p)
   (%even-out-target :initform 0.0
                     :type float
                     :accessor even-out-target)))

(defmethod mode ((camera camera))
  (car (slot-value camera '%mode)))

(defmethod mode-desc ((camera camera))
  (car (slot-value camera '%mode-desc)))

(defun next-mode (camera)
  (setf (slot-value camera '%mode) (cdr (slot-value camera '%mode))
        (slot-value camera '%mode-desc) (cdr (slot-value camera '%mode-desc)))
  (mode camera))

(defun update-player (player env-items delta)
  (when (is-key-down-p +key-left+) (decf (x player) (* 200.0 delta)))
  (when (is-key-down-p +key-right+) (incf (x player) (* 200.0 delta)))
  (when (and (is-key-down-p +key-space+) (can-jump-p player))
    (setf (spd player) -350.0
          (can-jump-p player) nil))
  (loop with hit-obstacle = nil
        for item in env-items
        do (when (and (blocking-p item)
                      (<= (x item) (+ (x player) 20))
                      (>= (+ (x item) (width item))
                          (+ (x player) 20))
                      (>= (y item) (+ (y player) 40))
                      (< (y item) (+ (y player) 40 (* (spd player) delta))))
             (setf hit-obstacle t
                   (spd player) 0.0
                   (y player) (- (y item) 40)))
        finally (if hit-obstacle
                    (setf (can-jump-p player) t)
                    (progn
                      (incf (y player) (* (spd player) delta))
                      (incf (spd player) (* 400.0 delta))
                      (setf (can-jump-p player) nil)))))

(defun update-camera-center (camera player width height)
  (setf (x (offset camera)) (/ width 2.0)
        (y (offset camera)) (/ height 2.0)
        (x (target camera)) (+ (x player) 20)
        (y (target camera)) (+ (y player) 40)))

(defun update-camera-center-inside-map (camera player env-items width height)
  (setf (x (target camera)) (+ (x player) 20)
        (y (target camera)) (+ (y player) 40)
        (x (offset camera)) (/ width 2.0)
        (y (offset camera)) (/ height 2.0))
  (loop with min-x = 1000.0
        with min-y = 1000.0
        with max-x = -1000.0
        with max-y = -1000.0
        for item in env-items
        do (setf min-x (min (x item) min-x)
                 max-x (max (+ (x item) (width item)) max-x)
                 min-y (min (y item) min-y)
                 max-y (max (+ (y item) (height item)) max-y))
        finally (let ((max (get-world-to-screen-2d (make-vector2 max-x max-y) camera))
                      (min (get-world-to-screen-2d (make-vector2 min-x min-y) camera)))
                  (when (< (x max) width)
                    (setf (x (offset camera)) (- width (- (x max) (/ width 2)))))
                  (when (< (y max) height)
                    (setf (y (offset camera)) (- height (- (y max) (/ height 2)))))
                  (when (> (x min) 0)
                    (setf (x (offset camera)) (- (/ width 2) (x min))))
                  (when (> (y min) 0)
                    (setf (y (offset camera)) (- (/ height 2) (y min)))))))

(defun update-camera-center-smooth-follow (camera player delta width height)
  (setf (x (offset camera)) (/ width 2.0)
        (y (offset camera)) (/ height 2.0))
  (let* ((diff (make-vector2 (- (+ (x player) 20) (x (target camera)))
                             (- (+ (y player) 40) (y (target camera)))))
         (len (vector2-length diff)))
    (when (> len 10.0)
      ;; This is equivalent to (setf (target camera) ...)
      ;; It works because we call VECTOR2-ADD destructively, modifying the first argument.
      (vector2-add (target camera)
                   (vector2-scale diff
                                  (* (max (* 0.8 len) 30.0)
                                     (/ delta len)))))))

(defun update-camera-even-out-on-landing (camera player delta width height)
  (setf *camera* camera
        *player* player)
  (setf (x (offset camera)) (/ width 2.0)
        (y (offset camera)) (/ height 2.0)
        (x (target camera)) (+ (x player) 20))
  (if (even-out-p camera)
      (if (> (even-out-target camera) (y (target camera)))
          (progn
            (incf (y (target camera)) (* (even-out-speed camera) delta))
            (when (> (y (target camera)) (even-out-target camera))
              (setf (y (target camera)) (even-out-target camera)
                    (even-out-p camera) nil)))
          (progn
            (decf (y (target camera)) (* (even-out-speed camera) delta))
            (when (< (y (target camera)) (even-out-target camera))
              (setf (y (target camera)) (even-out-target camera)
                    (even-out-p camera) nil))))
      (when (and (can-jump-p player)
                 (= 0 (spd player))
                 (/= (+ (y player) 40) (y (target camera))))
        (setf (even-out-p camera) t
              (even-out-target camera) (+ (y player) 40)))))

(defun update-camera-player-bounds-push (camera player width height)
  (let ((bbox-world-min (get-screen-to-world-2d
                         (make-vector2 (* 0.4 width) (* 0.4 height))
                         camera))
        (bbox-world-max (get-screen-to-world-2d
                         (make-vector2 (* 0.6 width) (* 0.6 height))
                         camera)))
    (setf (x (offset camera)) (* 0.4 width)
          (y (offset camera)) (* 0.4 height))
    (when (< (+ (x player) 20) (x bbox-world-min))
      (setf (x (target camera)) (+ (x player) 20)))
    (when (< (+ (y player) 40) (y bbox-world-min))
      (setf (y (target camera)) (+ (y player) 40)))
    (when (> (+ (x player) 20) (x bbox-world-max))
      (setf (x (target camera))
            (+ (x bbox-world-min)
               (- (+ (x player) 20) (x bbox-world-max)))))
    (when (> (+ (y player) 40) (y bbox-world-max))
      (setf (y (target camera))
            (+ (y bbox-world-min)
               (- (+ (y player) 40) (y bbox-world-max)))))))

(defun make-env-item (x y width height blocking-p color)
  (make-instance 'env-item
                 :x x
                 :y y
                 :width width
                 :height height
                 :blocking-p blocking-p
                 :color color))

(defun main ()
  (with-window (:title "raylib [core] example - 2d camera")
    (let* ((scene
             (make-scene ()
                         ((player (make-instance 'player
                                                 :x 380
                                                 :y 240
                                                 :spd 0
                                                 :can-jump-p nil
                                                 :width 40
                                                 :height 40
                                                 :color +red+))
                          (t1 (make-text "Controls:" 20 20 :size 10 :color +black+))
                          (t2 (make-text "- Right/Left to move"
                                         40 40
                                         :size 10 :color +darkgray+))
                          (t3 (make-text "- Space to jump"
                                         40 60
                                         :size 10 :color +darkgray+))
                          (t4 (make-text "- Mouse Wheel to Zoom in-out, R to reset zoom"
                                         40 80
                                         :size 10 :color +darkgray+))
                          (t5 (make-text "- C to change camera mode"
                                         40 100
                                         :size 10 :color +darkgray+))
                          (t6 (make-text "Current camera mode:"
                                         20 120
                                         :size 10 :color +black+))
                          (t7 (make-text "Follow player center"
                                         40 140
                                         :size 10 :color +darkgray+)))))
           (env-items (list (make-env-item 0 0 1000 400 nil +lightgray+)
                            (make-env-item 0 400 1000 200 t +gray+)
                            (make-env-item 300 200 400 10 t +gray+)
                            (make-env-item 250 300 100 10 t +gray+)
                            (make-env-item 650 300 100 10 t +gray+)))
           (camera (make-instance 'camera
                                  :target (make-vector2 400 280)
                                  :offset (make-vector2 (/ *screen-width* 2.0)
                                                        (/ *screen-height* 2.0))
                                  :rot 0.0
                                  :zoom 1.0)))
      (dolist (item env-items)
        (setf (gethash (gensym "ENV-ITEM") (objects scene)) item))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (let ((delta (get-frame-time))
                (player (scene-object scene 'player)))
            (update-player player env-items delta)
            (incf (zoom camera) (* (get-mouse-wheel-move) 0.05))
            (cond ((> (zoom camera) 3.0) (setf (zoom camera) 3.0))
                  ((< (zoom camera) 0.25) (setf (zoom camera) 0.25)))
            (when (is-key-pressed-p +key-r+)
              (setf (zoom camera) 1.0
                    (x player) 380
                    (y player) 240))
            (when (is-key-pressed-p +key-c+)
              (next-mode camera))
            (ecase (mode camera)
              (:center (update-camera-center camera
                                             player
                                             *screen-width*
                                             *screen-height*))
              (:center-inside-map (update-camera-center-inside-map camera
                                                                   player
                                                                   env-items
                                                                   *screen-width*
                                                                   *screen-height*))
              (:center-smooth-follow (update-camera-center-smooth-follow camera
                                                                         player
                                                                         delta
                                                                         *screen-width*
                                                                         *screen-height*))
              (:even-out-on-landing (update-camera-even-out-on-landing camera
                                                                       player
                                                                       delta
                                                                       *screen-width*
                                                                       *screen-height*))
              (:player-bounds-push (update-camera-player-bounds-push camera
                                                                     player
                                                                     *screen-width*
                                                                     *screen-height*)))
            (setf (text (scene-object scene 't7)) (mode-desc camera))
            (with-drawing (:bgcolor +lightgray+)
              (with-2d-mode camera
                (draw-scene-regex scene "^ENV-ITEM")
                (draw-scene scene 'player))
              (draw-scene-regex scene "^T[0-9]"))))))))
