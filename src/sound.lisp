(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-sound (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:sound))))

(defcreader astream rl-sound stream sound)
(defcreader frame-count rl-sound frame-count sound)

(defcwriter frame-count rl-sound frame-count sound integer)

(definitializer rl-sound
  :pt-accessors ((frame-count integer))
  :unload (safe-unload-sound t))

(defmethod play ((audio rl-sound))
  (claylib/ll:play-sound-multi (c-ptr audio)))

(defmethod stop ((audio rl-sound))
  (claylib/ll:stop-sound (c-ptr audio)))

(defmethod pause ((audio rl-sound))
  (claylib/ll:pause-sound (c-ptr audio)))

(defmethod resume ((audio rl-sound))
  (claylib/ll:resume-sound (c-ptr audio)))



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
  (claylib/ll:set-sound-pitch (c-ptr class)
                              (coerce new-value 'single-float)))

(defmethod (setf volume) :after (new-value (class sound))
  (claylib/ll:set-sound-volume (c-ptr class)
                               (coerce new-value 'single-float)))

(defmethod (setf pan) :after (new-value (class sound))
  (claylib/ll:set-sound-pan (c-ptr class)
                            (coerce new-value 'single-float)))

(definitializer sound
  :lisp-slots ((%pitch) (%volume)))
