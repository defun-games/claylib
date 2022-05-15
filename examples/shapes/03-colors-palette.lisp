(in-package #:claylib/examples)

(defun check-collision-point-rec (point rectangle)
  "Check if POINT is inside RECTANGLE.
TODO Is there an easier way using CLAYLIB/LL:CHECK-COLLISION-POINT-REC?"
  (let ((width (width rectangle))
        (height (height rectangle))
        (x (x rectangle))
        (y (y rectangle)))
    (and
     (< x (x point) (+ x width))
     (< y (y point) (+ y height)))))

(defun example-shapes-03 ()
  (with-window (:title "raylib [shapes] example - Colors pallete")
    (let* ((colors (list +darkgray+ +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+
                         +darkbrown+ +gray+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+
                         +pink+ +yellow+ +green+ +skyblue+ +purple+ +beige+))
           (color-names '("DARKGRAY" "MAROON" "ORANGE" "DARKGREEN" "DARKBLUE" "DARKPURPLE"
                          "DARKBROWN" "GRAY" "RED" "GOLD" "LIME" "BLUE" "VIOLET" "BROWN" "LIGHTGRAY"
                          "PINK" "YELLOW" "GREEN" "SKYBLUE" "PURPLE" "BEIGE"))
           (color-recs (loop for color in colors
                             for i = 0 then (1+ i)
                             collect (make-rectangle
                                      (+ 20
                                         (* 100 (mod i 7))
                                         (* 10 (mod i 7)))
                                      (+ 80
                                         (* 100 (truncate i 7))
                                         (* 10 (truncate i 7)))
                                      100
                                      100
                                      color)))
           (black-recs (loop for rect in color-recs
                             collect (make-rectangle (x rect)
                                                     (+ (y rect)
                                                        (height rect)
                                                        (- 26))
                                                     (width rect)
                                                     20
                                                     +black+)))
           (outline-recs (loop for rect in color-recs
                               collect (make-rectangle (x rect)
                                                       (y rect)
                                                       (width rect)
                                                       (height rect)
                                                       (fade +black+ 0.3 t)
                                                       :filled nil
                                                       :thickness 6)))
           (text-labels (loop for rect in color-recs
                              for color in colors
                              for color-name in color-names
                              collect (make-text color-name
                                                 (+ (x rect)
                                                    (width rect)
                                                    (- (measure-text color-name 10))
                                                    (- 12))
                                                 (+ (y rect) (height rect) (- 20))
                                                 :size 10
                                                 :color color)))
           (scene (make-scene ()
                              `((text ,(make-text "claylib colors palette"
                                                  28
                                                  42
                                                  :size 20
                                                  :color +black+))
                                (text-space ,(make-text "press SPACE to see all colors"
                                                        (- (get-screen-width) 180)
                                                        (- (get-screen-height) 40)
                                                        :size 10
                                                        :color +gray+))))))
      (with-scene scene ()
        (do-game-loop (:livesupport t
                       :vars ((color-state (make-array (length colors)
                                                       :element-type 'bit
                                                       :initial-element 0))
                              (mouse-point (make-vector2 0 0))
                              (faded-rect-color nil)))
          (get-mouse-position mouse-point)

          ;; restore original color if there was a faded rect
          (when faded-rect-color
            (setf (color (car faded-rect-color)) (cdr faded-rect-color)
                  faded-rect-color nil))

          (with-drawing
            (draw-scene-all scene)
            (loop for rect in color-recs
                  for black-rect in black-recs
                  for outline-rect in outline-recs
                  for label in text-labels
                  for i from 0 below (length color-recs)
                  for mouse-over-p = (check-collision-point-rec mouse-point rect)
                  do (when mouse-over-p
                       (setf faded-rect-color `(,rect . ,(color rect))) ; remember fade
                       (setf (color rect) (fade (color rect) 0.6 t)))
                     (draw-object rect)
                     (when (or (is-key-down-p +key-space+) mouse-over-p)
                       (draw-object black-rect)
                       (draw-object outline-rect)
                       (draw-object label)))))))))
