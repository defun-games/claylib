(in-package #:claylib)

(default-unload claylib/ll:music unload-music-stream t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-music ()
    ((%c-struct
      :type claylib/ll:music
      :accessor c-struct))
    (:default-initargs
     :c-struct (autowrap:calloc 'claylib/ll:music))))

(defcreader      astream     rl-music stream      music)
(defcreader      frame-count rl-music frame-count music)
(defcreader      ctx-type    rl-music ctx-type    music)
(defcreader      ctx-data    rl-music ctx-data    music)
(defcreader-bool looping     rl-music looping     music)

(defcwriter frame-count rl-music frame-count music integer)
(defcwriter ctx-type rl-music ctx-type music integer)
(defcwriter-bool looping rl-music looping music)

(definitializer rl-music
  :pt-accessors ((frame-count integer)
                 (looping boolean)
                 (ctx-type integer)))

(defmethod update ((object rl-music))
  (claylib/ll:update-music-stream (c-struct object)))

(defmethod play ((audio rl-music))
  (claylib/ll:play-music-stream (c-struct audio)))

(defmethod stop ((audio rl-music))
  (claylib/ll:stop-music-stream (c-struct audio)))

(defmethod pause ((audio rl-music))
  (claylib/ll:pause-music-stream (c-struct audio)))

(defmethod resume ((audio rl-music))
  (claylib/ll:resume-music-stream (c-struct audio)))



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
  (claylib/ll:set-music-pitch (c-struct class)
                              (coerce new-value 'single-float)))

(defmethod (setf volume) :after (new-value (class music))
  (claylib/ll:set-music-volume (c-struct class)
                               (coerce new-value 'single-float)))

(defmethod (setf pan) :after (new-value (class music))
  (claylib/ll:set-music-pan (c-struct class)
                            (coerce new-value 'single-float)))

(definitializer music
  :lisp-slots ((%pitch) (%volume)))
