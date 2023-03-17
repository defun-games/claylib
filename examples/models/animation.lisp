(in-package #:cl-user)
(defpackage claylib/examples/animation
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/animation)

(defparameter *assets*
  (list (make-model-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guy.iqm"))
        (make-texture-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guytex.png"))
        (make-animation-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guyanim.iqm"))))

(defparameter *scene*
  (make-scene ((model-asset (car *assets*))
               (model-texture (cadr *assets*))
               (model-anims (caddr *assets*)))
              ((camera (make-camera-3d 10 10 10
                                       0 0 0
                                       0 1 0
                                       :mode +camera-first-person+))
               (model (let ((m (make-model model-asset
                                           0 0 0
                                           :animation-asset model-anims
                                           :rot-axis (make-vector3 1 0 0)
                                           :rot-angle -90
                                           :tint +white+)))
                        (set-material-texture (elt (materials m) 0)
                                              +material-map-diffuse+
                                              (asset model-texture))
                        m))
               (grid (make-grid 10 1))
               (instructions (make-text "PRESS SPACE to PLAY MODEL ANIMATION"
                                        10 10
                                        :size 20
                                        :color +maroon+))
               (copyright (make-text "(c) Guy IQM 3D model by @culacant"
                                     (- (get-screen-width) 200) (- (get-screen-height) 20)
                                     :size 10
                                     :color +gray+)))))

(defun main ()
  (with-window (:title "raylib [models] example - model animation")
    (disable-cursor)
    (with-scenes *scene* ()
      (with-scene-objects (camera model) *scene*
        (do-game-loop (:livesupport t
                       :vars ((anim-frame-counter 0)))
          (update-camera camera)
          (when (is-key-down-p +key-space+)
            (incf anim-frame-counter)
            (update-model-animation model 0 anim-frame-counter)
            (when (>= anim-frame-counter (frame-count (elt (animations model) 0)))
              (setf anim-frame-counter 0)))
          (with-drawing ()
            (with-3d-mode camera
              (loop for i below (length (bones model))
                    ;; Sometimes it makes sense to call on low-level claylib (claylib/ll)
                    do (draw-cube (trans (elt (elt (frame-poses (elt (animations model) 0))
                                                   anim-frame-counter)
                                              i))
                                  0.2 0.2 0.2
                                  +red+))
              (draw-scene *scene* '(model grid)))
            (draw-scene *scene* '(instructions copyright))))))))
