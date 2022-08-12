(in-package #:claylib)

(defclass rl-music ()
  ((%c-struct
    :type claylib/ll:music
    :initform (autowrap:alloc 'claylib/ll:music)
    :accessor c-struct)))

(defcreader astream     rl-music stream      music)
(defcreader frame-count rl-music frame-count music)
(defcreader looping     rl-music looping     music)
(defcreader ctx-type    rl-music ctx-type    music)
(defcreader ctx-data    rl-music ctx-data    music)

(defcwriter-bool looping rl-music looping music)

(definitializer rl-music
    (frame-count integer) (looping boolean))




(defmacro with-audio-device (&body body)
  `(unwind-protect (progn (claylib/ll:init-audio-device)
                          ,@body)
     (claylib/ll:close-audio-device)))

;; TODO support LoadMusicStreamFromMemory
(defmacro with-music-stream (binding load-path &body body)
  `(progn
     (let ((,binding (make-instance 'rl-music))) ; TODO support music stream stuff
       (unwind-protect (progn (claylib/ll:load-music-stream (c-struct ,binding)
                                                            (namestring ,load-path))
                              ,@body)
         (claylib/ll:unload-music-stream (c-struct ,binding))))))
