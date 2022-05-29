(in-package #:claylib)

(defclass game-asset ()
  ((%path :initarg :path
          :type pathname
          :accessor path)
   (%asset
    :initform nil
    :accessor asset)))

(defreader c-asset game-asset c-struct asset)
(defwriter c-asset game-asset c-struct asset)

(defmethod free ((asset game-asset))
  (when (and (slot-boundp asset '%asset)
             (asset asset))
    (free (asset asset)))
  (setf (slot-value asset '%asset) nil)
  (when (next-method-p)
    (call-next-method)))

(defmethod initialize-instance :after ((asset game-asset) &key load-now)
  (when load-now (load-asset asset)))



(defclass image-asset (game-asset)
  ((%asset :type (or rl-image null))))

(defmethod load-asset ((asset image-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((img (make-instance 'rl-image)))
       (claylib/ll:load-image (c-struct img) (namestring (path asset)))
       (setf (asset asset) img)))
    (force-reload
     (claylib/ll:load-image (c-asset asset) (namestring (path asset)))))
  asset)



(defclass texture-asset (game-asset)
  ((%asset :type (or rl-texture null))))

(defmethod load-asset ((asset texture-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((tex (make-instance 'rl-texture)))
       (claylib/ll:load-texture (c-struct tex) (namestring (path asset)))
       (setf (asset asset) tex)))
    (force-reload
     (claylib/ll:load-texture (c-asset asset) (namestring (path asset)))))
  asset)



(defclass model-asset (game-asset)
  ((%asset :type (or rl-model null))))

(defmethod load-asset ((asset model-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((model (make-instance 'rl-model)))
       (claylib/ll:load-model (c-struct model) (namestring (path asset)))
       (setf (asset asset) model)))
    (force-reload
     (claylib/ll:load-model (c-asset asset) (namestring (path asset)))))
  asset)



(defclass shader-asset (game-asset)
  ((%vspath :initarg :vspath
            :type (or pathname null)
            :accessor vspath)
   (%fspath :initarg :fspath
            :type (or pathname null)
            :accessor fspath)
   (%asset :type (or rl-shader null))))

(default-slot-value shader-asset %vspath nil)
(default-slot-value shader-asset %fspath nil)

(defmethod load-asset ((asset shader-asset) &key force-reload)
  (let ((vpath (when (vspath asset)
                 (namestring (vspath asset))))
        (fpath (when (fspath asset)
                 (namestring (fspath asset)))))
    (cond
      ((null (asset asset))
       (let ((shader (make-instance 'rl-shader)))
         (claylib/ll:load-shader (c-struct shader) vpath fpath)
         (setf (asset asset) shader)))
      (force-reload
       (claylib/ll:load-shader (c-asset asset) vpath fpath))))
  asset)



(defclass font-asset (game-asset)
  ((%font-size :initarg :size
               :type integer
               :accessor size)
   (%font-chars :initarg :chars
                :type integer
                :accessor chars)
   (%glyph-count :initarg :glyph-count
                 :type integer
                 :accessor glyph-count)
   (%asset :type (or rl-font null))))

(default-slot-value font-asset %font-size 10)
(default-slot-value font-asset %font-chars 0)
(default-slot-value font-asset %glyph-count 224)

(defmethod load-asset ((asset font-asset) &key force-reload)
  (flet ((load-it (font asset)
           (if (or (slot-boundp asset '%font-size)
                   (slot-boundp asset '%font-chars)
                   (slot-boundp asset '%glyph-count))
               (claylib/ll:load-font-ex font
                                        (namestring (path asset))
                                        (size asset)
                                        (chars asset)
                                        (glyph-count asset))
               (claylib/ll:load-font font (namestring (path asset))))))
    (cond
      ((null (asset asset))
       (let ((font (make-instance 'rl-font)))
         (load-it (c-struct font) asset)
         (setf (asset asset) font)))
      (force-reload
       (load-it (c-asset asset) asset))))
  asset)



(defclass animation-asset (game-asset)
  ((%num 
    :type integer
    :accessor num)
   (%asset :type (or rl-model-animation null))))

(defmethod load-asset ((asset animation-asset) &key force-reload)
  (cond
    ((null (asset asset))
     (let ((anim (make-instance 'rl-model-animation)))
       (c-let ((i :int))
         (setf (c-struct anim) (load-model-animations (namestring (path asset)) (i &))
               (num asset) i
               (asset asset) anim))))
    (force-reload
     (c-let ((i :int))
       (setf (c-asset asset)
             (load-model-animations (namestring (path asset)) (i &))
             (num asset) i))))
  asset)

(defmethod free ((asset animation-asset))
  (when (and (asset asset)
             (autowrap:valid-p (c-asset asset)))
    (claylib/ll:unload-model-animations (c-asset asset) (num asset))
    (autowrap:free (c-asset asset)))
  (setf (slot-value asset '%asset) nil)
  (when (next-method-p)
    (call-next-method)))
