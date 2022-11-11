(in-package #:cl-user)
(defpackage claylib/examples/mouse-painting
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/mouse-painting)

(defun main ()
  (with-window (:title "raylib [textures] example - mouse painting"
                :fps 120)
    (let* ((screen-width (get-screen-width))
           (screen-height (get-screen-height))
           (colors (list +raywhite+ +yellow+ +gold+ +orange+ +pink+ +red+ +maroon+ +green+ +lime+
                         +darkgreen+ +skyblue+ +blue+ +darkblue+ +purple+ +violet+ +darkpurple+
                         +beige+ +brown+ +darkbrown+ +lightgray+ +gray+ +darkgray+ +black+))
           (color-swaths (loop for color in colors
                               for i = 0 then (1+ i)
                               collect (make-rectangle (+ 10 (* 30.0 i) (* 2 i))
                                                       10
                                                       30
                                                       30
                                                       color)))
           (swath-outlines (loop for rec in color-swaths
                                 collect (make-rectangle (- (x rec) 2) (- (y rec) 2)
                                                         (+ (width rec) 4) (+ (height rec) 4)
                                                         +black+
                                                         :thickness 2
                                                         :filled nil)))
           (target (let* ((width screen-width)
                          (height screen-height)
                          (rt (load-render-texture width height))
                          (source (make-rectangle 0 0 width (- height) +white+))
                          (dest (make-rectangle 0 0 width height +white+)))
                     (setf (width (texture rt)) width
                           (height (texture rt)) height
                           (tint (texture rt)) *claylib-background*
                           (source (texture rt)) source
                           (dest (texture rt)) dest)
                     rt))
           (brush (make-circle (get-mouse-x) (get-mouse-y) 20.0 +black+))
           (eraser (make-circle (get-mouse-x) (get-mouse-y) 20.0 +raywhite+))
           (eraser-outline (make-circle (get-mouse-x) (get-mouse-y) 20.0 +gray+ :filled nil))

           (scene (make-scene
                   ()
                   ((top-panel (make-rectangle 0 0
                                               screen-width 50
                                               +raywhite+))
                    (top-panel-line (make-line-2d 0 50 screen-width 50 +lightgray+))
                    (white-swath-outline (make-rectangle 10 10 30 30 +lightgray+ :filled nil))
                    (btn-save-rec (make-rectangle 750 10 40 30 +black+ :filled nil))
                    (btn-save-msg (make-text "SAVE!" 755 20 :size 10 :color +black+))
                    (save-rec1 (make-rectangle 0 0
                                               screen-width screen-height
                                               (fade +raywhite+ 0.8 t)))
                    (save-rec2 (make-rectangle 0 150
                                               screen-width 80
                                               +black+))
                    (save-text (make-text "IMAGE SAVED: my_amazing_texture_painting.png"
                                          150 180
                                          :size 20
                                          :color +raywhite+))))))
      (with-scenes scene ()
        ;; Clear render texture before entering the game loop
        (with-texture-mode (target))
        (do-game-loop (:livesupport t
                       :vars ((swath-selected (first color-swaths))
                              (swath-selected-prev (first color-swaths))
                              (swath-hovered nil)
                              (mouse-was-pressed nil)
                              (btn-save-mouse-hover nil)
                              (show-save-message nil)
                              (save-message-counter 0)
                              (mouse-pos (make-vector2 0 0))
                              (max-colors-count (1- (length colors)))))
          (get-mouse-position :vec mouse-pos)

          ;; Move between colors with keys
          (cond ((is-key-pressed-p +key-right+)
                 (setf swath-selected
                       (nth (min max-colors-count (1+ (position swath-selected color-swaths)))
                            color-swaths)))
                ((is-key-pressed-p +key-left+)
                 (setf swath-selected
                       (nth (max 0 (1- (position swath-selected color-swaths)))
                            color-swaths))))

          ;; Choose color with mouse
          (setf swath-hovered
                (loop for rec in color-swaths
                      for swath-under-mouse = (if (check-collision-point-rec mouse-pos rec)
                                                  rec
                                                  nil)
                      when swath-under-mouse
                        return swath-under-mouse))

          (when (and swath-hovered (is-mouse-button-pressed-p +mouse-button-left+))
            (setf swath-selected swath-hovered
                  swath-selected-prev swath-selected))

          ;; Clear render texture to clear color
          (when (is-key-pressed-p +key-c+)
            (with-texture-mode (target)))

          ;; Update brush size, color & position
          (let ((radius (min 50 (max 2 (+ (radius brush)
                                          (* (get-mouse-wheel-move) 5)))))
                (x (x mouse-pos))
                (y (y mouse-pos)))
            (setf (color brush) (color swath-selected))
            (mapc (lambda (obj) (setf (radius obj) radius
                                      (x obj) x
                                      (y obj) y))
                  (list brush eraser eraser-outline)))

          ;; Paint circle into render texture
          (when (or (is-mouse-button-down-p +mouse-button-left+)
                    (= (get-gesture-detected) +gesture-drag+))
            (with-texture-mode (target :clear nil)
              (when (> (y mouse-pos) 50)
                (draw-object brush))))

          (if (is-mouse-button-down-p +mouse-button-right+)
              (progn
                (unless mouse-was-pressed
                  (setf swath-selected-prev swath-selected
                        swath-selected (first color-swaths)))
                (setf mouse-was-pressed t)

                ;; Erase circle from render texture
                (with-texture-mode (target :clear nil)
                  (when (> (y mouse-pos) 50)
                    (draw-object eraser))))

              (when (and (is-mouse-button-released-p +mouse-button-right+)
                         mouse-was-pressed)
                (setf swath-selected swath-selected-prev
                      mouse-was-pressed nil)))

          (with-scene-objects (btn-save-rec btn-save-msg save-rec1 save-rec2 save-text
                                            white-swath-outline) scene
            ;; Check if the mouse is hovering over the save button, change color accordingly
            (let ((color (if (setf btn-save-mouse-hover
                                   (check-collision-point-rec mouse-pos btn-save-rec))
                             +red+
                             +black+)))
              (setf (color btn-save-rec) color
                    (color btn-save-msg) color))

            ;; Image saving logic
            (when (or (and btn-save-mouse-hover
                           (is-mouse-button-released-p +mouse-button-left+))
                      (is-key-pressed-p +key-s+))
              (let ((image (load-image-from-texture (texture target))))
                (export-image (image-flip-vertical image)
                              (claylib/examples:claylib-path
                               "examples/textures/my_amazing_texture_painting.png"))
                (setf show-save-message t)))

            (when (and show-save-message (> (incf save-message-counter) 240))
              (setf show-save-message nil
                    save-message-counter 0))

            (with-drawing ()
              ;; Draw render texture
              (draw-object (texture target))

              ;; Draw drawing circle for reference
              (when (> (y mouse-pos) 50)
                (if (is-mouse-button-down-p +mouse-button-right+)
                    (draw-object eraser-outline)
                    (draw-object brush)))

              ;; Draw top panel
              (draw-scene-regex scene "^TOP-PANEL*")

              ;; Draw color selection rectangles
              (mapc #'draw-object color-swaths)
              (draw-object white-swath-outline)
              (draw-object (nth (position swath-selected color-swaths) swath-outlines))

              ;; Draw save image button
              (draw-scene-regex scene "^BTN-SAVE*")

              ;; Draw save image message
              (when show-save-message
                (draw-scene-regex scene "^SAVE-*")))))))))
