(in-package #:claylib)

(default-unload claylib/ll:music unload-music-stream t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rl-music ()
    ((%c-struct :type claylib/ll:music
                :initform (autowrap:alloc 'claylib/ll:music)
                :accessor c-struct)
     (%pitch :initarg :pitch
             :type float
             :reader pitch)
     (%volume :initarg :volume
              :type float
              :reader volume))))

(defcreader astream      rl-music stream      music)
(defcreader frame-count  rl-music frame-count music)
(defcreader ctx-type     rl-music ctx-type    music)
(defcreader ctx-data     rl-music ctx-data    music)
(defcreader-bool looping rl-music looping music)

(defcwriter-bool looping rl-music looping music)

(defwriter-float pitch rl-music)
(defwriter-float volume rl-music)

(defmethod (setf pitch) :after (new-value (class rl-music))
  (claylib/ll:set-music-pitch (c-struct class)
                              (coerce new-value 'single-float)))

(defmethod (setf volume) :after (new-value (class rl-music))
  (claylib/ll:set-music-volume (c-struct class)
                               (coerce new-value 'single-float)))

(definitializer rl-music
  :lisp-slots ((%pitch) (%volume))
  :pt-accessors ((frame-count integer) (looping boolean) (ctx-type integer) #|(ctx-data pointer)|#))

(default-slot-value rl-music %pitch 1.0)
(default-slot-value rl-music %volume 1.0)

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
