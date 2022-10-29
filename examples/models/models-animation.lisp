(in-package #:cl-user)
(defpackage claylib/examples/models-animation
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/models-animation)

(defparameter *assets*
  (list (claylib::make-model-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guy.iqm"))
        (make-texture-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guytex.png"))
        (claylib::make-animation-asset
         (claylib/examples:claylib-path "examples/models/resources/models/iqm/guyanim.iqm"))))

(defparameter *scene*
  (make-scene ((model-asset (car *assets*))
               (model-texture (cadr *assets*))
               ;(model-anims (caddr *assets*))  ; TODO: This line causes a hang on quit
               )
              ((camera (make-camera-3d 10 10 10
                                       0 0 0
                                       0 1 0
                                       :mode +camera-free+))
               (model (let ((m (claylib::make-model model-asset     ; TODO: This form causes a double free on quit
                                                    0 0 0
                                                    :rot-axis (make-vector3 1 0 0)
                                                    :rot-angle -90
                                                    :tint +white+)))
                        (claylib/wrap:set-material-texture
                         (autowrap:ptr (claylib::c-struct (elt (claylib::materials m) 0)))
                         claylib/wrap:+material-map-diffuse+
                         (claylib::c-asset model-texture))
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
    (with-scenes *scene*
      (with-scene-objects (camera) *scene*
        (do-game-loop (:livesupport t
                       :vars ((anim-frame-counter 0)))
          (update-camera camera)
          ;; TODO: Animations go here
          (with-drawing ()
            (with-3d-mode camera
              (draw-scene *scene* 'model 'grid))
            (draw-scene *scene* 'instructions 'copyright)))))))
