(in-package #:claylib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-music (c-struct linkable)
    ()
    (:default-initargs
     :c-ptr (calloc 'claylib/ll:music))))

(defcreader      astream     rl-music stream      music)
(defcreader      frame-count rl-music frame-count music)
(defcreader      ctx-type    rl-music ctx-type    music)
(defcreader      ctx-data    rl-music ctx-data    music)
(defcreader      looping     rl-music looping     music)

(define-print-object rl-music
  (pitch volume astream frame-count ctx-type ctx-data looping))

(defcwriter frame-count rl-music frame-count music integer)
(defcwriter ctx-type rl-music ctx-type music integer)

(defcwriter looping rl-music looping music)

(definitializer rl-music
  :pt-accessors ((frame-count integer)
                 (looping boolean)
                 (ctx-type integer))
  :unload (safe-unload-music t))

(defmethod update ((object rl-music))
  (claylib/ll:update-music-stream (c-ptr object)))

(defmethod play ((audio rl-music))
  (claylib/ll:play-music-stream (c-ptr audio)))

(defmethod stop ((audio rl-music))
  (claylib/ll:stop-music-stream (c-ptr audio)))

(defmethod pause ((audio rl-music))
  (claylib/ll:pause-music-stream (c-ptr audio)))

(defmethod resume ((audio rl-music))
  (claylib/ll:resume-music-stream (c-ptr audio)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass music (rl-music)
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

(defwriter-float pitch music)
(defwriter-float volume music)
(defwriter-float pan music)

(defmethod (setf pitch) :after (new-value (class music))
  (claylib/ll:set-music-pitch (c-ptr class)
                              (coerce new-value 'single-float)))

(defmethod (setf volume) :after (new-value (class music))
  (claylib/ll:set-music-volume (c-ptr class)
                               (coerce new-value 'single-float)))

(defmethod (setf pan) :after (new-value (class music))
  (claylib/ll:set-music-pan (c-ptr class)
                            (coerce new-value 'single-float)))

(definitializer music
  :lisp-slots ((%pitch) (%volume)))
