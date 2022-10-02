(in-package #:claylib/ll)

(defun array-valid-p (pointer array-len autowrap-type)
  "Check validity of a C pointer-array. Does *not* test individual elements, only pointer sanity."
  (let ((maxptr (1- (expt 2 64))))
    (and (not (cffi:null-pointer-p pointer))
         (>= (- maxptr (cffi:pointer-address pointer))
             (* array-len (autowrap:foreign-type-size autowrap-type))))))

(defgeneric data-valid-p (wrapper)
  (:documentation "Determine whether a wrapper is valid and its data can be safely read.

*** FAIR WARNING: Some of these checks are more robust than others. ***"))

(defmethod data-valid-p :around ((wrapper autowrap:wrapper))
  (if (and (autowrap:valid-p wrapper)
           (not (cffi:null-pointer-p (autowrap:ptr wrapper))))
      (call-next-method)
      ;; Null-pointer-wrappers are never valid.
      ;; Quit early before C throws a hissy fit.
      nil))

(defmethod data-valid-p ((wrapper autowrap:wrapper)) t)

(defmethod data-valid-p ((wrapper rectangle))
  (and (> (rectangle.width wrapper) 0)
       (> (rectangle.height wrapper) 0)))

(defmethod data-valid-p ((wrapper image))
  (and (> (image.width wrapper) 0)
       (> (image.height wrapper) 0)
       (> (image.mipmaps wrapper) 0)
       ;; Current Raylib pixel format types correspond to ints 1-21
       (>= (image.format wrapper) 1)
       (<= (image.format wrapper) 21)))

(defmethod data-valid-p ((wrapper texture))
  (and (> (texture.width wrapper) 0)
       (> (texture.height wrapper) 0)
       (> (texture.mipmaps wrapper) 0)
       ;; Current Raylib pixel format types correspond to ints 1-21
       (>= (texture.format wrapper) 1)
       (<= (texture.format wrapper) 21)))

(defmethod data-valid-p ((wrapper render-texture))
  (or (data-valid-p (render-texture.texture wrapper))
      (data-valid-p (render-texture.depth wrapper))))

(defmethod data-valid-p ((wrapper glyph-info))
  (and (>= (glyph-info.value wrapper) 0)
       (data-valid-p (glyph-info.image wrapper))))

(defmethod data-valid-p ((wrapper font))
  (and (> (font.base-size wrapper) 0)
       (> (font.glyph-count wrapper) 0)
       (>= (font.glyph-padding wrapper) 0)
       (data-valid-p (font.texture wrapper))))

(defmethod data-valid-p ((wrapper camera3d))
  (or (= (camera3d.projection wrapper) 0)
      (= (camera3d.projection wrapper) 1)))

(defmethod data-valid-p ((wrapper camera2d))
  (> (camera2d.zoom wrapper) 0))

(defmethod data-valid-p ((wrapper mesh))
  (let ((vcount (mesh.vertex-count wrapper)))
    (and (> vcount 0)
         (> (mesh.triangle-count wrapper) 0)
         (array-valid-p (mesh.vertices wrapper) (* vcount 3) :float)
         (array-valid-p (mesh.texcoords wrapper) (* vcount 2) :float)
         (array-valid-p (mesh.normals wrapper) (* vcount 3) :float))))

(defmethod data-valid-p ((wrapper shader))
  (let ((locs (shader.locs wrapper))
        (max-shader-locs 32))
    (and (array-valid-p locs max-shader-locs :int)
         ;; Current Raylib shader loc values correspond to ints 0-25
         (>= (autowrap:c-aref locs 0 :int) 0)
         (<= (autowrap:c-aref locs 0 :int) 25))))

(defmethod data-valid-p ((wrapper material-map))
  (data-valid-p (material-map.texture wrapper)))

(defmethod data-valid-p ((wrapper material))
  (let ((maps (material.maps wrapper))
        (max-material-maps 12))
    (and (array-valid-p maps max-material-maps 'material-map)
         (data-valid-p (material.shader wrapper))
         (data-valid-p (autowrap:c-aref maps 0 'material-map)))))

(defmethod data-valid-p ((wrapper model))
  (let ((mcount (model.mesh-count wrapper)))
    (and (> mcount 0)
         (array-valid-p (model.meshes wrapper) mcount 'mesh))))

(defmethod data-valid-p ((wrapper model-animation))
  (and (> (model-animation.bone-count wrapper) 0)
       (> (model-animation.frame-count wrapper) 0)
       (not (cffi:null-pointer-p (model-animation.bones wrapper)))
       (not (cffi:null-pointer-p (model-animation.frame-poses wrapper)))))

(defmethod data-valid-p ((wrapper wave))
  (and (> (wave.frame-count wrapper) 0)
       (> (wave.sample-rate wrapper) 0)
       (let ((sampsize (wave.sample-size wrapper)))
         (or (= sampsize 8)
             (= sampsize 16)
             (= sampsize 32)))
       (> (wave.channels wrapper) 0)))

(defmethod data-valid-p ((wrapper audio-stream))
  (and (> (audio-stream.sample-rate wrapper) 0)
       (let ((sampsize (audio-stream.sample-size wrapper)))
         (or (= sampsize 8)
             (= sampsize 16)
             (= sampsize 32)))
       (> (audio-stream.channels wrapper) 0)))

(defmethod data-valid-p ((wrapper sound))
  (and (> (sound.frame-count wrapper) 0)
       (data-valid-p (sound.stream wrapper))))

(defmethod data-valid-p ((wrapper music))
  (and (> (music.frame-count wrapper) 0)
       (data-valid-p (music.stream wrapper))))

(defmethod data-valid-p ((wrapper vr-device-info))
  (and (> (vr-device-info.h-resolution wrapper) 0)
       (> (vr-device-info.v-resolution wrapper) 0)
       (> (vr-device-info.h-screen-size wrapper) 0)
       (> (vr-device-info.v-screen-size wrapper) 0)))

(defmethod data-valid-p ((wrapper file-path-list))
  (and (> (file-path-list.capacity wrapper) 0)
       (>= (file-path-list.count wrapper) 0)))
