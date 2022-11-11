(in-package #:cl-user)
(defpackage claylib/examples/colors-palette
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/colors-palette)

(defun main ()
  (with-window (:title "raylib [shapes] example - Colors pallete")
    (let* ((colors (list +darkgray+ +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+
                         +darkbrown+ +gray+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+
                         +pink+ +yellow+ +green+ +skyblue+ +purple+ +beige+))
           (color-names '("DARKGRAY" "MAROON" "ORANGE" "DARKGREEN" "DARKBLUE" "DARKPURPLE"
                          "DARKBROWN" "GRAY" "RED" "GOLD" "LIME" "BLUE" "VIOLET" "BROWN" "LIGHTGRAY"
                          "PINK" "YELLOW" "GREEN" "SKYBLUE" "PURPLE" "BEIGE"))
           (faded-colors (loop for color in colors
                               collect (fade color 0.6 t)))
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
           (label-recs (loop for rect in color-recs
                             collect (make-rectangle (x rect)
                                                     (+ (y rect) (height rect) -26)
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
                              ((crecs color-recs)
                               (lrecs label-recs)
                               (orecs outline-recs)
                               (tlabels text-labels)
                               (text (make-text "claylib colors palette"
                                                28
                                                42
                                                :size 20
                                                :color +black+))
                               (text-space (make-text "press SPACE to see all colors"
                                                      (- (get-screen-width) 180)
                                                      (- (get-screen-height) 40)
                                                      :size 10
                                                      :color +gray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t
                       :vars ((mouse-point (make-vector2 0 0))
                              (faded-rect-color nil)))
          (get-mouse-position :vec mouse-point)

          (with-scene-objects (crecs lrecs orecs tlabels text text-space) scene
            (with-drawing ()
              (draw-object text)
              (draw-object text-space)
              (loop for rect in crecs
                    for color in colors
                    for faded-color in faded-colors
                    for label-rect in lrecs
                    for outline-rect in orecs
                    for label in tlabels
                    for i below (length crecs)
                    for mouse-over-p = (check-collision-point-rec mouse-point rect)
                    do (setf (color rect)
                             (if mouse-over-p faded-color color))
                       (draw-object rect)
                       (when (or (is-key-down-p +key-space+) mouse-over-p)
                         (mapc #'draw-object (list label-rect outline-rect label)))))))))))
