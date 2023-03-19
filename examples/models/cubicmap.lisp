(in-package #:cl-user)
(defpackage claylib/examples/cubicmap
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/cubicmap)

(defparameter *scene*
  (make-scene-pro ((:params
                    (camera (make-camera-3d 16 14 16
                                            0 0 0
                                            0 1 0
                                            :mode +camera-orbital+)))
                   (:assets
                    (image-ass (make-image-asset (claylib/examples:claylib-path
                                                  "examples/models/resources/cubicmap.png")))
                    (texture (make-texture-asset (claylib/examples:claylib-path
                                                  "examples/models/resources/cubicmap_atlas.png"))))
                   (:objects
                    (cubicmap (let ((tex (make-empty-texture
                                          :rot 0
                                          :tint +white+)))
                                (load-texture-from-image (asset image-ass) :texture tex)
                                (setf (source tex)
                                      (make-simple-rec 0 0 (width tex) (height tex))
                                      (dest tex)
                                      (make-simple-rec (- (get-screen-width) (* 4 (width tex)) 20) 20
                                                       (* 4 (width tex)) (* 4 (height tex))))
                                tex))
                    (model (let ((m (load-model-from-mesh (gen-mesh-cubicmap (asset image-ass)
                                                                             (make-vector3 1 1 1)))))
                             ;; Change the texture of the diffuse map of 0th material in the model
                             (set-material-texture (elt (materials m) 0)
                                                   +material-map-diffuse+
                                                   (asset texture))

                             (setf (x m) -16
                                   (y m) 0
                                   (z m) -8)
                             m))
                    (rec (make-rectangle (- (get-screen-width)
                                            (* 4 (width cubicmap))
                                            20)
                                         20
                                         (* 4 (width cubicmap))
                                         (* 4 (height cubicmap))
                                         +green+
                                         :filled nil))
                    (line1 (make-text "cubicmap image used to"
                                      658 90
                                      :size 10
                                      :color +gray+))
                    (line2 (make-text "generate map 3d model"
                                      658 104
                                      :size 10
                                      :color +gray+))))))

(defun main ()
  (with-window (:title "raylib [models] example - cubesmap loading and drawing")
    (with-scenes *scene* ()
      (with-scene-objects (model cubicmap rec line1 line2) *scene*
        (let ((camera (scene-param *scene* 'camera)))
          (do-game-loop (:livesupport t)
            (update-camera camera)
            (with-drawing ()
              (with-3d-mode camera
                (draw-object model))
              (draw-objects (list cubicmap rec line1 line2))
              (draw-fps 10 10))))))))
