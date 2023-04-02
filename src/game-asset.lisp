(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass game-asset ()
    ((%path :initarg :path
            :type pathname
            :accessor path)
     (%asset
      :initform nil
      :accessor asset))))

(defreader c-asset game-asset c-ptr asset)

(define-print-object game-asset
    (path asset))

;(defwriter c-asset game-asset c-ptr asset)

(defmethod initialize-instance :after ((asset game-asset) &key load-now)
  (when load-now (load-asset asset)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass image-asset (game-asset)
    ((%asset :type (or rl-image null)))))

(defreader data image-asset data asset)
(defreader width image-asset width asset)
(defreader height image-asset height asset)
(defreader mipmaps image-asset mipmaps asset)
(defreader data-format image-asset data-format asset)

(define-print-object image-asset
    (data width height mipmaps data-format))

(defmethod load-asset ((asset image-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((img (make-instance 'rl-image)))
       (claylib/ll:load-image (c-ptr img) (namestring (path asset)))
       (setf (asset asset) img)))
    (force-reload
     (claylib/ll:load-image (c-asset asset) (namestring (path asset)))))
  asset)

(defun make-image-asset (path &key (load-now nil))
  (make-instance 'image-asset :path path :load-now load-now))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass texture-asset (game-asset)
    ((%asset :type (or rl-texture null)))))

(defreader id texture-asset id asset)
(defreader width texture-asset width asset)
(defreader height texture-asset height asset)
(defreader mipmaps texture-asset mipmaps asset)
(defreader data-format texture-asset data-format asset)

(define-print-object texture-asset
    (id width height mipmaps data-format))

(defmethod load-asset ((asset texture-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((tex (make-instance 'rl-texture)))
       (claylib/ll:load-texture (c-ptr tex) (namestring (path asset)))
       (setf (asset asset) tex)))
    (force-reload
     (claylib/ll:load-texture (c-asset asset) (namestring (path asset)))))
  asset)

(defun make-texture-asset (path &key (load-now nil))
  "Make a texture asset from a PATH. This does not load the texture unless LOAD-NOW is non-nil."
  (make-instance 'texture-asset :path path :load-now load-now))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass model-asset (game-asset)
    ((%asset :type (or rl-model null)))))

(defreader mesh-count model-asset mesh-count asset)
(defreader material-count model-asset material-count asset)
(defreader bone-count model-asset bone-count asset)
(defreader transform model-asset transform asset)
(defreader meshes model-asset meshes asset)
(defreader materials model-asset materials asset)
(defreader mesh-materials model-asset mesh-materials asset)
(defreader bones model-asset bones asset)
(defreader bind-pose model-asset bind-pose asset)

(define-print-object model-asset
    (mesh-count material-count bone-count transform meshes materials mesh-materials bones bind-pose))

(defmethod mesh-material ((masset model-asset) (index integer))
  (mesh-material (asset masset) index))

(defwriter mesh-count model-asset mesh-count asset)
(defwriter material-count model-asset material-count asset)
(defwriter bone-count model-asset bone-count asset)
(defwriter transform model-asset transform asset)
(defwriter meshes model-asset meshes asset)
(defwriter materials model-asset materials asset)
(defwriter mesh-materials model-asset mesh-materials asset)
(defwriter bones model-asset bones asset)
(defwriter bind-pose model-asset bind-pose asset)
(defmethod (setf mesh-material) ((value integer) (masset model-asset) (index integer))
  (setf (mesh-material (asset masset) index) value))

(defmethod load-asset ((asset model-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((model (make-instance 'rl-model)))
       (claylib/ll:load-model (c-ptr model) (namestring (path asset)))
       (setf (asset asset) model)))
    (force-reload
     (claylib/ll:load-model (c-asset asset) (namestring (path asset)))))
  asset)

(defun make-model-asset (path &key (load-now nil))
  "Make a model asset from a PATH. This does not load the model unless LOAD-NOW is non-nil."
  (make-instance 'model-asset :path path :load-now load-now))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass shader-asset (game-asset)
    ((%vspath :initarg :vspath
              :type (or pathname null)
              :accessor vspath)
     (%fspath :initarg :fspath
              :type (or pathname null)
              :accessor fspath)
     (%asset :type (or rl-shader null)))
    (:default-initargs
     :vspath ""
     :fspath "")))

(defreader id shader-asset id asset)
(defreader locs shader-asset locs asset)

(define-print-object shader-asset
    (id vspath fspath locs))

(defmethod load-asset ((asset shader-asset) &key force-reload)
  (let ((vpath (when (vspath asset)
                 (namestring (vspath asset))))
        (fpath (when (fspath asset)
                 (namestring (fspath asset)))))
    (cond
      ((null (asset asset))
       (let ((shader (make-instance 'rl-shader)))
         (claylib/ll:load-shader (c-ptr shader) vpath fpath)
         (setf (asset asset) shader)))
      (force-reload
       (claylib/ll:load-shader (c-asset asset) vpath fpath))))
  asset)

(defun make-shader-asset (&rest initargs &key vspath fspath (load-now nil))
  "Make a shader asset from files VSPATH and FSPATH. This does not load the model unless LOAD-NOW is
non-nil."
  (declare (ignorable vspath fspath load-now))
  (apply #'make-instance 'shader-asset initargs))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass font-asset (game-asset)
    ((%font-size :initarg :size
                 :type integer
                 :writer (setf size))
     (%font-chars :initarg :chars
                  :type sequence
                  :accessor chars)
     (%glyph-count :initarg :glyph-count
                   :type integer
                   :writer (setf glyph-count))
     (%asset :type (or rl-font null)))))

(defmethod size ((obj font-asset))
  (if (asset obj)
      (size (asset obj))
      (slot-value obj '%font-size)))

(defmethod glyph-count ((obj font-asset))
  (if (asset obj)
      (glyph-count (asset obj))
      (slot-value obj '%glyph-count)))

(defreader glyph-padding font-asset glyph-padding asset)
(defreader texture font-asset texture asset)
(defreader recs font-asset recs asset)
(defreader glyphs font-asset glyphs asset)

(define-print-object font-asset
    (chars size glyph-count glyph-padding texture recs glyphs))

(default-slot-value font-asset %font-size 10)
(default-slot-value font-asset %font-chars nil)
(default-slot-value font-asset %glyph-count 224)

(defmethod load-asset ((asset font-asset) &key force-reload)
  (flet ((load-it (font asset)
           (if (or (slot-boundp asset '%font-size)
                   (slot-boundp asset '%font-chars)
                   (slot-boundp asset '%glyph-count))
               (cffi:with-foreign-object (c-array :int (glyph-count asset))
                 (flet ((c-char (char i)
                          (setf (cffi:mem-aref c-array :int i) (char-int char))))
                   (claylib/ll:load-font-ex font
                                            (namestring (path asset))
                                            (size asset)
                                            (etypecase (chars asset)
                                              (null (cffi:null-pointer))
                                              (list (loop for char in (chars asset)
                                                          for i from 0
                                                          collect (c-char char i)
                                                          finally (return c-array)))
                                              (array (loop for char across (chars asset)
                                                           for i from 0
                                                           collect (c-char char i)
                                                           finally (return c-array))))
                                            (glyph-count asset))))
               (claylib/ll:load-font font (namestring (path asset))))))
    (cond
      ((null (asset asset))
       (let ((font (make-instance 'rl-font)))
         (load-it (c-ptr font) asset)
         (setf (asset asset) font)))
      (force-reload
       (load-it (c-asset asset) asset))))
  asset)

(defun make-font-asset (path &rest args &key size chars glyph-count (load-now nil))
  (declare (ignorable size chars glyph-count load-now))
  (apply #'make-instance 'font-asset :path path args))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass animation-asset (game-asset)
    ((%asset :type (or rl-animations null)))))

;; (defreader bone-count animation-asset bone-count asset)
;; (defreader frame-count animation-asset frame-count asset)
;; (defreader bones animation-asset bones asset)
;; (defreader frame-poses animation-asset frame-poses asset)

(define-print-object animation-asset
  ())

(defmethod load-asset ((asset animation-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let* ((i (calloc :int))
            (0th-anim (load-model-animations (namestring (path asset)) i))
            (anims (make-instance 'rl-animations
                                  :cl-array (make-rl-model-animation-array
                                             0th-anim (cffi:mem-ref i :int)))))
       (setf (asset asset) anims)))
    ;; TODO how best to force-reload?
    (force-reload
     (let ((i (calloc :int)))
       (setf (slot-value asset '%c-asset)
             (load-model-animations (namestring (path asset)) i)
                                        ;(num asset) (cffi:mem-ref i :int)
             ))))
  asset)

(defun make-animation-asset (path &key (load-now nil))
  "Make an animation asset from a PATH. This does not load the model unless LOAD-NOW is non-nil."
  (make-instance 'animation-asset :path path :load-now load-now))



(defclass music-asset (game-asset)
  ((%asset :type (or rl-music null))))

(define-print-object music-asset
  ())

;; Don't really need this
;; (defreader pitch music-asset pitch asset)

(defmethod load-asset ((asset music-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((music (make-instance 'music)))
       (claylib/ll:load-music-stream (c-ptr music) (namestring (path asset)))
       (setf (asset asset) music)))
    (force-reload
     (claylib/ll:load-music-stream (c-asset asset) (namestring (path asset)))))
  asset)

(defun make-music-asset (path &key (load-now nil))
  (make-instance 'music-asset :path path :load-now load-now))



(defclass sound-asset (game-asset)
  ((%asset :type (or rl-sound null))))

(defmethod load-asset ((asset sound-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((sound (make-instance 'sound)))
       (claylib/ll:load-sound (c-ptr sound) (namestring (path asset)))
       (setf (asset asset) sound)))
    (force-reload
     (claylib/ll:load-sound (c-asset asset) (namestring (path asset)))))
  asset)

(defun make-sound-asset (path &key (load-now nil))
  (make-instance 'sound-asset :path path :load-now load-now))



(defmethod load-asset ((asset list) &key force-reload)
  (dolist (ass asset)
    (load-asset ass :force-reload force-reload)))
