(in-package #:claylib)

(default-unload claylib/ll:sound unload-sound t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-sound ()
    ((%c-struct
      :type claylib/ll:sound
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:sound))))

(defcreader astream rl-sound stream sound)
(defcreader frame-count rl-sound frame-count sound)

(defcwriter frame-count rl-sound frame-count sound integer)

(definitializer rl-sound
  :pt-accessors ((frame-count integer)))

(defmethod play ((audio rl-sound))
  (claylib/ll:play-sound-multi (c-struct audio)))

(defmethod stop ((audio rl-sound))
  (claylib/ll:stop-sound (c-struct audio)))

(defmethod pause ((audio rl-sound))
  (claylib/ll:pause-sound (c-struct audio)))

(defmethod resume ((audio rl-sound))
  (claylib/ll:resume-sound (c-struct audio)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sound (rl-sound)
    ((%pitch :initarg :pitch
             :type float
             :reader pitch)
     (%volume :initarg :volume
              :type float
              :reader volume)
     (%pan :initarg :pan
           :type float
           :reader pan))
    (:default-initargs
     :pitch 1.0
     :volume 1.0
     :pan 0.5)))

(defwriter-float pitch sound)
(defwriter-float volume sound)
(defwriter-float pan sound)

(defmethod (setf pitch) :after (new-value (class sound))
  (claylib/ll:set-sound-pitch (c-struct class)
                              (coerce new-value 'single-float)))

(defmethod (setf volume) :after (new-value (class sound))
  (claylib/ll:set-sound-volume (c-struct class)
                               (coerce new-value 'single-float)))

(defmethod (setf pan) :after (new-value (class sound))
  (claylib/ll:set-sound-pan (c-struct class)
                            (coerce new-value 'single-float)))

(definitializer sound
  :lisp-slots ((%pitch) (%volume)))
