(in-package #:cl-user)
(defpackage claylib/examples/logo-raylib-anim
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/logo-raylib-anim)

(defparameter *faded-black* (copy-color +black+))
(defparameter *faded-white* (copy-color +raywhite+))

(defun make-side-rec (&key (x-offset 0) (y-offset 0))
  "Make a rectangle representing a side of the raylib logo."
  (make-rectangle (+ (- (/ (get-screen-width) 2) 128) x-offset)
                  (+ (- (/ (get-screen-height) 2) 128) y-offset)
                  16 16
                  *faded-black*))

(defparameter *scene*
  (make-scene ()
              ((top-side-rec (make-side-rec))
               (left-side-rec (make-side-rec))
               (right-side-rec (make-side-rec :x-offset 240))
               (bottom-side-rec (make-side-rec :y-offset 240))
               (text (make-text ""
                                (- (/ (get-screen-width) 2) 44)
                                (+ (/ (get-screen-height) 2) 48)
                                :size 50
                                :color *faded-black*))
               (white-rec (make-rectangle (- (/ (get-screen-width) 2) 112)
                                          (- (/ (get-screen-height) 2) 112)
                                          224 224
                                          *faded-white*))
               (replay-text (make-text "[R] REPLAY" 340 200 :size 20 :color +gray+)))))

(defun main ()
  (with-window (:title "raylib [shapes] example - raylib logo animation")
    (with-scenes *scene* ()
      (with-scene-objects (top-side-rec left-side-rec bottom-side-rec right-side-rec text white-rec
                                        replay-text) *scene*
          (do-game-loop (:livesupport t
                         :vars ((frames-counter 0)
                                (letters-count 0)
                                (state 0)
                                (alpha 1.0)))
            (case state
              ;; Small box blinking
              (0 (progn (incf frames-counter)
                        (when (= frames-counter 120) (setf frames-counter 0
                                                           state 1))))
              ;; Top and left bars growing
              (1 (progn (incf (width top-side-rec) 4)
                        (incf (height left-side-rec) 4)
                        (when (= (width top-side-rec) 256) (setf state 2))))
              ;; Bottom and right bars growing
              (2 (progn (incf (width bottom-side-rec) 4)
                        (incf (height right-side-rec) 4)
                        (when (= (width bottom-side-rec) 256) (setf state 3))))
              ;; Letters appearing (one by one)
              (3 (progn (incf frames-counter)
                        (when (= (rem frames-counter 12) 0)
                          (incf letters-count)
                          (setf frames-counter 0))
                        (when (>= letters-count 10)
                          (decf alpha 0.02)
                          (when (<= alpha 0) (setf alpha 0
                                                   state 4)))
                        (setf (text text) (subseq "raylib" 0 (min letters-count 6)))))
              ;; Reset and Replay
              (4 (when (is-key-pressed-p +key-r+)
                   (setf frames-counter 0
                         letters-count 0
                         (width top-side-rec) 16
                         (height left-side-rec) 16
                         (width bottom-side-rec) 16
                         (height right-side-rec) 16
                         (text text) ""
                         alpha 1.0
                         state 0))))

            ;; Change the colour fading
            (fade *faded-black* alpha)
            (fade *faded-white* alpha)

            (with-drawing ()
              (case state
                (0 (when (= 0 (mod (truncate frames-counter 15) 2))
                     (draw-object top-side-rec)))
                (1 (draw-objects (list top-side-rec left-side-rec)))
                (2 (draw-objects (list top-side-rec left-side-rec right-side-rec bottom-side-rec)))
                (3 (draw-objects (list top-side-rec left-side-rec right-side-rec bottom-side-rec
                                       white-rec text)))
                (4 (draw-object replay-text)))))))))
