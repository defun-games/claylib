(in-package #:cl-user)
(defpackage claylib/examples/sprite-anim
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/sprite-anim)

(defun main ()
  (with-window (:title "raylib [texture] example - sprite anim"
                :fps 60)
    (let* ((scarfy-asset (make-texture-asset (claylib/examples:claylib-path
                                              "examples/textures/resources/scarfy.png")
                                             :load-now t))
           (scarfy-width (width scarfy-asset))
           (scarfy-height (height scarfy-asset))
           (max-frame-speed 15)
           (scene (make-scene ()
                              ((scarfy-full (make-texture scarfy-asset
                                                          15 40
                                                          :tint +white+))
                               (scarfy-frame (make-texture scarfy-asset
                                                           350 280
                                                           :source (make-simple-rec
                                                                    0 0
                                                                    (/ scarfy-width 6)
                                                                    scarfy-height)
                                                           :width (/ scarfy-width 6)
                                                           :height scarfy-height
                                                           :tint +white+))
                               (lime-outline (make-rectangle 15 40
                                                             scarfy-width
                                                             scarfy-height
                                                             +lime+
                                                             :filled nil))
                               (red-outline (make-rectangle 0 0
                                                            (/ scarfy-width 6) scarfy-height
                                                            +red+
                                                            :filled nil))
                               (frame-speed-text (make-text "FRAME SPEED: "
                                                            165 210
                                                            :size 10
                                                            :color +darkgray+))
                               (fps-text (make-text "00 FPS"
                                                    575 210
                                                    :size 10
                                                    :color +darkgray+))
                               (fps-recs (loop for i below max-frame-speed
                                               collect (make-rectangle (+ 250 (* 21 i)) 205
                                                                       20 20
                                                                       +maroon+
                                                                       :filled nil)))
                               (hint-text (make-text "PRESS RIGHT/LEFT KEYS to CHANGE SPEED!"
                                                     290 240
                                                     :size 10
                                                     :color +darkgray+))
                               (copyright-text (make-text "(c) Scarfy sprite by Eiden Marsal"
                                                          (- (get-screen-width) 200)
                                                          (- (get-screen-height) 20)
                                                          :size 10
                                                          :color +gray+))))))
      (with-scenes scene ()
        (do-game-loop (:livesupport t
                       :vars ((current-frame 0)
                              (frames-counter 0 (+ 1 frames-counter))
                              (frames-speed 8)))

          (when (>= frames-counter (truncate 60 frames-speed))
            (setf frames-counter 0)
            (when (> (incf current-frame) 5)
              (setf current-frame 0))
            (setf (x (source (scene-object scene 'scarfy-frame)))
                  (* current-frame (/ scarfy-width 6))))

          (if (is-key-pressed-p +key-right+)
              (incf frames-speed)
              (when (is-key-pressed-p +key-left+)
                (decf frames-speed)))
          (setf frames-speed (min max-frame-speed (max 1 frames-speed)))

          (with-scene-objects (red-outline fps-text scarfy-frame fps-recs) scene
            (setf (x red-outline) (+ 15 (x (source scarfy-frame)))
                  (y red-outline) (+ 40 (y (source scarfy-frame)))
                  (text fps-text) (format nil "~2,'0d FPS" frames-speed))
            (loop for rec in fps-recs
                  for i below max-frame-speed
                  do (if (< i frames-speed)
                         (setf (color rec) +red+
                               (filled rec) t)
                         (setf (color rec) +maroon+
                               (filled rec) nil))))

          (with-drawing ()
            (draw-scene-all scene)))))))
