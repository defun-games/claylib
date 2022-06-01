(in-package #:cl-user)
(defpackage claylib/examples/textures-3
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/textures-3)

(defun main ()
  (with-window (:title "raylib [texture] example - texture rectangle"
                :fps 60)
    (let* ((scarfy-asset (make-texture-asset (asdf:system-relative-pathname
                                              :claylib
                                              "examples/textures/resources/scarfy.png")
                                             :load-now t))
           (scarfy-width (width scarfy-asset))
           (scarfy-height (height scarfy-asset))
           (scene (make-scene ()
                              `((scarfy-full ,(make-texture scarfy-asset
                                                            15 40
                                                            :tint +white+))
                                (scarfy-frame ,(make-texture scarfy-asset
                                                             350 280
                                                             :source (make-rectangle
                                                                      0 0
                                                                      (/ scarfy-width 6)
                                                                      scarfy-height
                                                                      +black+)
                                                             ;; :width (/ scarfy-width 6)
                                                             ;; :height scarfy-height
                                                             :tint +white+))
                                (lime-outline ,(make-rectangle 15 40
                                                            scarfy-width
                                                            scarfy-height
                                                            +lime+
                                                            :filled nil))
                                (red-outline ,(make-rectangle 0 0
                                                              0 0
                                                              +red+
                                                              :filled nil))
                                (frame-speed-text ,(make-text "FRAME SPEED: "
                                                              165 210
                                                              :size 10
                                                              :color +darkgray+))
                                (fps-text ,(make-text "00 FPS"
                                                      575 210
                                                      :size 10
                                                      :color +darkgray+))
                                (hint-text ,(make-text "PRESS RIGHT/LEFT KEYS to CHANGE SPEED!"
                                                       290 240
                                                       :size 10
                                                       :color +darkgray+))
                                (copyright-text ,(make-text "(c) Scarfy sprite by Eiden Marsal"
                                                            (- (get-screen-width) 200)
                                                            (- (get-screen-height) 20)
                                                            :size 10
                                                            :color +gray+))))))
      (do-game-loop (:livesupport t
                     :vars ((current-frame 0)
                            (frames-counter 0)
                            (frames-speed 8)))
        (incf frames-counter)

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
        (setf frames-speed (min 15 (max 1 frames-speed)))

        (with-scene-objects (red-outline fps-text scarfy-frame) scene
            (setf (x red-outline) (+ 15 (x (source scarfy-frame)))
                  (y red-outline) (+ 40 (y (source scarfy-frame)))
                  (width red-outline) (width (source scarfy-frame)) ; FIXME unnecessary SETFs each time
                  (height red-outline) (height (source scarfy-frame)) ; here too
                  (text fps-text) (format nil "~2,'0d FPS" frames-speed)))

        (with-drawing
          (draw-scene-all scene))))))