(in-package #:cl-user)
(defpackage claylib/examples/collision-area
  (:use :cl :claylib)
  (:import-from :alexandria
   :clamp)
  (:export :main))
(in-package #:claylib/examples/collision-area)

(defconstant +screen-upper-limit+ 40)

(defparameter *scene*
  (flet ((half-sub (x y)
           "Subtract Y from half of X."
           (- (/ x 2) y)))
    (make-scene ()
                ((collision-indicator (make-rectangle 0 0
                                                      *screen-width* +screen-upper-limit+
                                                      +black+))
                 (box-a (make-rectangle 10 (half-sub *screen-height* 50)
                                        200 100
                                        +gold+))
                 (box-b (make-rectangle (half-sub *screen-width* 30) (half-sub *screen-height* 30)
                                        60 60
                                        +blue+))
                 (box-collision (make-rectangle 0 0 0 0 +lime+))
                 (collision-message (make-text "COLLISION!"
                                               (half-sub *screen-width*
                                                         (/ (measure-text "COLLISION!" 20) 2))
                                               (half-sub +screen-upper-limit+ 10)
                                               :size 20
                                               :color +black+))
                 (collision-area (make-text ""
                                            (half-sub *screen-width* 100)
                                            (+ +screen-upper-limit+ 10)
                                            :size 20
                                            :color +black+))))))

(defun past-x-bounds-p (box)
  "Whether BOX is located beyond the x screen limits."
  (or (>= (+ (x box) (width box))
          (get-screen-width))
      (<= (x box) 0)))

(defun box-to-mouse (box)
  "Move BOX to the mouse position without exceeding the screen boundaries."
  (setf (x box) (clamp (- (get-mouse-x) (/ (width box) 2))
                       0
                       (- (get-screen-width) (width box)))
        (y box) (clamp (- (get-mouse-y) (/ (height box) 2))
                       +screen-upper-limit+
                       (- (get-screen-height) (height box)))))

(defun main ()
  (with-window (:title "raylib [shapes] example - collision area")
    (with-scenes *scene* ()
      (with-scene-objects (collision-indicator box-a box-b box-collision collision-message
                                               collision-area) *scene*
        (do-game-loop (:livesupport t
                       :vars ((box-a-speed-x 4)
                              (pause nil)
                              (collision nil)))

          ;; Move Box A if not paused
          (unless pause (incf (x box-a) box-a-speed-x))

          ;; Bounce Box A on x screen limits
          (when (past-x-bounds-p box-a) (setf box-a-speed-x (- box-a-speed-x)))

          ;; Update Box B based on mouse
          (box-to-mouse box-b)

          ;; Check box collision
          (setf collision (check-collision-recs box-a box-b))

          (when collision
            (get-collision-rec box-a box-b :result-rec box-collision)
            (setf (text collision-area) (format nil "Collision Area: ~d"
                                                (* (width box-collision) (height box-collision)))
                  (color collision-indicator) +red+))

          (unless collision
            (setf (color collision-indicator) +black+))

          ;; Pause Box A movement
          (when (is-key-pressed-p +key-space+) (setf pause (not pause)))

          (with-drawing ()
            (draw-objects (list collision-indicator box-a box-b))
            (when collision
              (draw-objects (list box-collision collision-message collision-area)))
            (draw-fps 10 10)))))))
