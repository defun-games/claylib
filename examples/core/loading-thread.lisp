(in-package #:cl-user)
(defpackage claylib/examples/loading-thread
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/loading-thread)

(defparameter *data-progress* 0)

(defparameter *scene*
  (make-scene ()
              ((text-start (make-text "PRESS ENTER to START LOADING DATA"
                                      150 170
                                      :size 20
                                      :color +darkgray+))
               (progress-bar (make-rectangle 150 200 *data-progress* 60 +skyblue+))
               (text-loading (make-text "LOADING DATA..." 240 210 :size 40 :color +darkblue+))
               (text-loaded (make-text "DATA LOADED!" 250 210 :size 40 :color +green+))
               (border (make-rectangle 150 200 500 60 +darkgray+ :filled nil)))))

(defun load-data ()
  "Simulate data loading. Updates the special variable *DATA-PROGRESS* according to the time this
function has been running."
  (do ((counter 0)
       (start-time (get-internal-real-time)))
      ((>= counter 5) (setf *data-progress* 500))
    (setf counter (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)
          *data-progress* (* counter 100))))

(defun should-flash-p (frame)
  "Whether FRAME is a frame that we should draw flashing objects."
  (= 0 (mod (truncate frame 15) 2)))

(defun main ()
  (with-window (:title "raylib [core] example - loading thread")
    (with-scenes *scene* ()
      (with-scene-objects (progress-bar) *scene*
        (do-game-loop (:livesupport t
                       :vars ((state :waiting)
                              (frames-counter 0)
                              (thread nil)))
          (case state
            (:waiting (when (is-key-pressed-p +key-enter+)
                        (setf thread (bt:make-thread #'load-data)
                              (color progress-bar) +skyblue+
                              state :loading)))
            (:loading (progn (incf frames-counter)
                             (setf (width progress-bar) *data-progress*)
                             (unless (bt:thread-alive-p thread)
                               (setf frames-counter 0
                                     (color progress-bar) +lime+
                                     state :finished))))
            (:finished (when (is-key-pressed-p +key-enter+)
                         (setf *data-progress* 0
                               state :waiting))))
          (with-drawing ()
            (case state
              (:waiting (draw-scene *scene* '(text-start border)))
              (:loading (draw-scene *scene*
                                    `(progress-bar
                                      ,(when (should-flash-p frames-counter) 'text-loading)
                                      border)))
              (:finished (draw-scene *scene* '(progress-bar text-loaded border))))))))))
