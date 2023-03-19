(in-package #:cl-user)
(defpackage claylib/examples/3d-camera-first-person
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/3d-camera-first-person)

(defvar *mouse-delta* (make-vector2 0 0))

(defun reset-up (camera)
  (setf (x (up camera)) 0
        (y (up camera)) 1
        (z (up camera)) 0))

(defun pro-mvmt (key1 key2)
  (if (or (is-key-down-p key1) (is-key-down-p key2)) 0.1 0))

(defun pro-mode-update (camera)
  (get-mouse-delta :vec *mouse-delta*)
  (setf (x (movement camera)) (- (pro-mvmt +key-w+ +key-up+)
                                 (pro-mvmt +key-s+ +key-down+))
        (y (movement camera)) (- (pro-mvmt +key-d+ +key-right+)
                                 (pro-mvmt +key-a+ +key-left+))
        (x (rot camera)) (* 0.05 (x *mouse-delta*))
        (y (rot camera)) (* 0.05 (y *mouse-delta*))
        (zoom camera) (* 2 (get-mouse-wheel-move))))

(defun compute-camera (camera)
  (cond
    ((is-key-pressed-p +key-one+)
     (setf (mode camera) +camera-free+)
     (reset-up camera))
    ((is-key-pressed-p +key-two+)
     (setf (mode camera) +camera-first-person+)
     (reset-up camera))
    ((is-key-pressed-p +key-three+)
     (setf (mode camera) +camera-third-person+)
     (reset-up camera))
    ((is-key-pressed-p +key-four+)
     (setf (mode camera) +camera-orbital+)
     (reset-up camera))
    ((is-key-pressed-p +key-five+)
     (setf (mode camera) +camera-pro+)
     (reset-up camera))
    ((is-key-pressed-p +key-p+)
     (setf (mode camera) +camera-third-person+
           (x camera) 0
           (y camera) 2
           (x (target camera)) 0
           (y (target camera)) 2
           (z (target camera)) 0)
     (reset-up camera)
     (if (= (projection camera) +camera-perspective+)
         (progn
           (setf (z camera) -100
                 (projection camera) +camera-orthographic+
                 (fovy camera) 20)
           (camera-yaw camera (* -135 (/ pi 180)) t)
           (camera-pitch camera (* -45 (/ pi 180)) t nil))
         (setf (z camera) 10
               (projection camera) +camera-perspective+
               (fovy camera) 60))))
  (when (= (mode camera) +camera-pro+)
    (pro-mode-update camera))
  (update-camera camera))

(defun main ()
  (with-window (:title "raylib [core] example - 3d camera first person")
    (let ((camera (make-camera-3d 0 2 4
                                  0 2 0
                                  0 1 0
                                  :fovy 60.0
                                  :projection +camera-perspective+
                                  :mode +camera-first-person+))
          (scene (make-scene ()
                             ((ground (make-plane 0 0 0 32 32 +lightgray+))
                              (blue (make-cube -16 2.5 0
                                               1 5 32
                                               +blue+))
                              (green (make-cube 16 2.5 0
                                                1 5 32
                                                +lime+))
                              (yellow (make-cube 0 2.5 16
                                                 32 5 1
                                                 +gold+))
                              (inst-box (make-rectangle 5 5 330 100 (fade +skyblue+ 0.5 t)))
                              (inst-border (make-rectangle 5 5 330 100 +blue+ :filled nil))
                              (inst1 (make-text "Camera controls:" 15 15 :size 10 :color +black+))
                              (inst2 (make-text "- Move keys: W, A, S, D, Space, Left-Ctrl"
                                                15 30
                                                :size 10 :color +black+))
                              (inst3 (make-text "- Look around: arrow keys or mouse"
                                                15 45
                                                :size 10 :color +black+))
                              (inst4 (make-text "- Camera mode keys: 1, 2, 3, 4, 5"
                                                15 60
                                                :size 10 :color +black+))
                              (inst5 (make-text "- Zoom keys: num-plus, num-minus or mouse scroll"
                                                15 75
                                                :size 10 :color +black+))
                              (inst6 (make-text "- Camera projection key: P"
                                                15 90
                                                :size 10 :color +black+))
                              (status-box (make-rectangle 600 5 195 100 (fade +skyblue+ 0.5 t)))
                              (status-border (make-rectangle 600 5 195 100 +blue+ :filled nil))
                              (status1 (make-text "Camera status:" 610 15 :size 10 :color +black+))
                              (status2 (make-text "- Mode: " 610 30 :size 10 :color +black+))
                              (status3 (make-text "- Projection: " 610 45 :size 10 :color +black+))
                              (status4 (make-text "- Position: " 610 60 :size 10 :color +black+))
                              (status5 (make-text "- Target: " 610 75 :size 10 :color +black+))
                              (status6 (make-text "- Up: " 610 90 :size 10 :color +black+))))))
      (dotimes (i 20)
        (let* ((h (get-random-value 1 12))
               (pos (make-vector3 (get-random-value -15 15)
                                  (/ h 2.0)
                                  (get-random-value -15 15)))
               (color (make-color (get-random-value 20 255)
                                  (get-random-value 10 55)
                                  30))
               (size (make-vector3 2.0 h 2.0)))
          (setf (gethash (gensym "COLUMN") (objects scene))
                (make-cube-from-vecs pos size color)
                (gethash (gensym "COLUMN") (objects scene))
                (make-cube-from-vecs pos size +maroon+ :filled nil))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t)
          (compute-camera camera)
          (with-scene-objects (status2 status3 status4 status5 status6) scene
            (setf (text status2) (format nil "- Mode: ~A" (cond
                                                            ((= (mode camera) +camera-free+) "FREE")
                                                            ((= (mode camera) +camera-first-person+)
                                                             "FIRST_PERSON")
                                                            ((= (mode camera) +camera-third-person+)
                                                             "THIRD_PERSON")
                                                            ((= (mode camera) +camera-orbital+)
                                                             "ORBITAL")
                                                            ((= (mode camera) +camera-pro+) "PRO")
                                                            (t "CUSTOM")))
                  (text status3) (format nil "- Projection: ~A" (if (= (projection camera)
                                                                       +camera-perspective+)
                                                                    "PERSPECTIVE"
                                                                    "ORTHOGRAPHIC"))
                  (text status4) (format nil "- Position: (~,3f, ~,3f, ~,3f)"
                                         (x camera) (y camera) (z camera))
                  (text status5) (format nil "- Target: (~,3f, ~,3f, ~,3f)"
                                         (x (target camera)) (y (target camera)) (z (target camera)))
                  (text status6) (format nil "- Up: (~,3f, ~,3f, ~,3f)"
                                         (x (up camera)) (y (up camera)) (z (up camera)))))
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene scene '(ground blue green yellow))
              (draw-scene-regex scene "^COLUMN"))
            (draw-scene-regex scene "^(STATUS|INST)")))))))
