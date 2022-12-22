(in-package #:cl-user)
(defpackage claylib/examples/model-shader
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/model-shader)

;; Hard-coded GLSL_VERSION because Claylib bundles the desktop build of Raylib.
(defvar +glsl-vsn+ 330)

(defparameter *scene*
  (make-scene-pro
      ((:params
        (cam (make-camera-3d 4 4 4
                             0 1 -1
                             0 1 0
                             :mode +camera-free+)))
       (:assets
        (modass (make-model-asset
                 (claylib/examples:claylib-path "examples/shaders/resources/models/watermill.obj")))
        (texass (make-texture-asset
                 (claylib/examples:claylib-path
                  "examples/shaders/resources/models/watermill_diffuse.png")))
        (shdass (claylib::make-shader-asset
                 (claylib/examples:claylib-path
                  (format nil "examples/shaders/resources/shaders/glsl~d/grayscale.fs" +glsl-vsn+)))))
       (:objects
        (model (let ((m (make-model modass 0 0 0)))
                 (set-slot :shader (elt (materials m) 0) (asset shdass))
                 (print (texture (elt (maps (elt (materials m) 0)) +material-map-diffuse+)))
                 (print (asset texass))
                 (setf ;; (claylib::shader (elt (materials m) 0))
                       ;; (asset shdass)
                       (texture (elt (maps (elt (materials m) 0)) +material-map-diffuse+))
                       (asset texass))
                 m))
        (grid (make-grid 10 1))
        (txt (make-text "(c) Watermill 3D model by Alberto Cano"
                        (- (get-screen-width) 210)
                        (- (get-screen-height) 20)
                        :size 10
                        :color +gray+))))))

(defun main ()
  (with-window (:title "raylib [shaders] example - model shader"
                :flags (list +flag-msaa-4x-hint+))
    (with-scenes *scene* ()
      (with-scene-objects (model grid text) *scene*
        (let ((cam (scene-param *scene* 'cam)))
          (do-game-loop (:livesupport t)
            (update-camera cam)
            (with-drawing ()
              (with-3d-mode cam
                (draw-objects model grid))
              (draw-fps 10 10))))))))
