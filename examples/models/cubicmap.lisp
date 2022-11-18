(in-package #:cl-user)
(defpackage claylib/examples/cubicmap
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/cubicmap)

(defparameter *scene*
  (make-scene ((image-ass (make-image-asset (claylib/examples:claylib-path
                                             "examples/models/resources/cubicmap.png")))
               (texture (make-texture-asset (claylib/examples:claylib-path
                                             "examples/models/resources/cubicmap_atlas.png"))))
              ((cubicmap (let ((cubicmap (make-empty-texture
                                           ;; :origin (make-vector2 0 0)
                                           :rot 0
                                           :tint +white+)))
                           (load-texture-from-image (asset image-ass) :texture cubicmap)
                           (setf (source cubicmap) (make-simple-rec 0 0
                                                                    (width cubicmap)
                                                                    (height cubicmap))
                                 (dest cubicmap) (make-simple-rec (- (get-screen-width)
                                                                     (* (width cubicmap) 4))
                                                                  20
                                                                  (* 4 (width cubicmap))
                                                                  (* 4 (height cubicmap))))
                           cubicmap))
               (model (let ((m (load-model-from-mesh (gen-mesh-cubicmap (asset image-ass)
                                                                        (make-vector3 1 1 1)))))
                        ;; Change the texture of the diffuse map of 0th material in the model
                        (set-slot :texture
                                  (elt (claylib::maps (elt (materials m) 0)) +material-map-diffuse+)
                                  (asset texture))

                        (setf (pos m) (make-vector3 -16 0 -8))
                        ;; TODO Why doesn't this work?
                        ;; (setf (x m) -16
                        ;;       (y m) 0
                        ;;       (z m) -8)
                        m))
               (camera (make-camera-3d 16 14 16
                                       0 0 0
                                       0 1 0
                                       :mode +camera-orbital+))
               (line1 (make-text "cubicmap image used to"
                                 658 90
                                 :size 10
                                 :color +gray+))
               (line2 (make-text "generate map 3d model"
                                 658 104
                                 :size 10
                                 :color +gray+)))))

(defun main ()
  (with-window (:title "raylib [models] example - cubesmap loading and drawing")
    (with-scenes *scene* ()
      (with-scene-objects (camera model line1 line2) *scene*
        (do-game-loop (:livesupport t)
          (update-camera camera)
          (with-drawing ()
            (with-3d-mode camera
              (draw-object model))
            (draw-objects line1 line2)
            (draw-fps 10 10)))))))
