(in-package #:claylib)

(defclass game-asset ()
  ((%path :initarg :path
          :type pathname
          :accessor path)
   (%c-asset
    :accessor c-asset)))

(defmethod free ((asset game-asset))
  (when (and (slot-boundp asset '%c-asset)
             (c-asset asset)
             (autowrap:valid-p (c-asset asset)))
    (free (c-asset asset)))
  (setf (slot-value asset '%c-asset) nil)
  (tg:cancel-finalization asset)
  (when (next-method-p)
    (call-next-method)))

(defmethod initialize-instance :after ((asset game-asset) &key load-now)
  (when load-now (load-asset asset)))



(defclass image-asset (game-asset)
  ((%c-asset :type claylib/ll:image)))

(defmethod load-asset ((asset image-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:image))
      (claylib/ll:load-image c (path asset))
      (setf (c-asset asset) c)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
  asset)



(defclass texture-asset (game-asset)
  ((%c-asset :type claylib/ll:texture)))

(defmethod load-asset ((asset texture-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:texture))
      (claylib/ll:load-texture c (path asset))
      (setf (c-asset asset) c)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
  asset)



(defclass model-asset (game-asset)
  ((%c-asset :type claylib/ll:model)))

(defmethod load-asset ((asset model-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:model))
      (claylib/ll:load-model c (path asset))
      (setf (c-asset asset) c)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
  asset)



(defclass shader-asset (game-asset)
  ((%vspath :initarg :vspath
            :type (or pathname null)
            :accessor vspath)
   (%fspath :initarg :fspath
            :type (or pathname null)
            :accessor fspath)
   (%c-asset :type claylib/ll:shader)))

(default-slot-value shader-asset %vspath nil)
(default-slot-value shader-asset %fspath nil)

(defmethod load-asset ((asset shader-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:shader))
      (claylib/ll:load-shader c (vspath asset) (fspath asset))
      (setf (c-asset asset) c)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
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
   (%c-asset :type claylib/ll:font)))

(default-slot-value font-asset %font-size 10)
(default-slot-value font-asset %font-chars 0)
(default-slot-value font-asset %glyph-count 224)

(defmethod load-asset ((asset font-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:font))
      (if (or (slot-boundp asset '%font-size)
              (slot-boundp asset '%font-chars)
              (slot-boundp asset '%glyph-count))
          (claylib/ll:load-font-ex c (path asset) (size asset) (chars asset) (glyph-count asset))
          (claylib/ll:load-font c (path asset)))
      (setf (c-asset asset) c)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
  asset)



(defclass animation-asset (game-asset)
  ((%num 
    :type integer
    :accessor num)
   (%c-asset :type claylib/ll:model-animation)))

(defmethod load-asset ((asset animation-asset) &key force-reload)
  (when (and force-reload (c-asset asset))
    (free-later (c-asset asset))
    (tg:cancel-finalization asset))
  (when (or force-reload (null (c-asset asset)))
    (c-let ((c claylib/ll:model-animation)
            (i :int))
      (setf c (claylib/ll:load-model-animations (path asset) (i &))
            (c-asset asset) c
            (num asset) i)
      (tg:finalize asset
                   (let ((ptr (autowrap:ptr (c-asset asset))))
                     (lambda () (autowrap:free ptr))))))
  asset)

(defmethod free ((asset animation-asset))
  (when (and (c-asset asset)
             (autowrap:valid-p (c-asset asset)))
    (claylib/ll:unload-model-animations (c-asset asset) (num asset))
    (autowrap:free (c-asset asset)))
  (setf (slot-value asset '%c-asset) nil)
  (tg:cancel-finalization asset)
  (when (next-method-p)
    (call-next-method)))
