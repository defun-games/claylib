(in-package #:cl-user)
(defpackage claylib/examples/rectangle-scaling
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/rectangle-scaling)

(defconstant +mouse-scale-mark-size+ 12)

(defun drag-rec-component (component rec)
  "Calculate either the :x or :y COMPONENT of the DRAG-REC based on the current size of REC."
  (+ 100
     (if (eq component :x) (width rec) (height rec))
     (- +mouse-scale-mark-size+)))

(defparameter *scene*
  (let ((x 100) (y 100) (w 200) (h 80))
    (make-scene ()
                ((rec (make-rectangle x y w h (fade +green+ 0.5 t)))
                 (mark-outline (make-rectangle x y w h +red+ :filled nil))
                 (mark (make-triangle (+ x w (- +mouse-scale-mark-size+)) (+ y h)
                                      (+ x w) (+ y h)
                                      (+ x w) (+ y h (- +mouse-scale-mark-size+))
                                      +red+))
                 (text (make-text "Scale rectangle dragging from bottom-right corner!"
                                  10 10
                                  :size 20
                                  :color +gray+))))))

(defun main ()
  (with-window (:title "raylib [shapes] example - rectangle scaling mouse")
    (with-scenes *scene* ()
      (with-scene-objects (rec mark-outline mark) *scene*
        (do-game-loop (:livesupport t
                       :vars ((mouse-position (make-vector2 0 0))
                              (mouse-scale-ready nil)
                              (mouse-scale-mode nil)
                              (drag-rec (make-rectangle
                                         (drag-rec-component :x rec) (drag-rec-component :y rec)
                                         +mouse-scale-mark-size+ +mouse-scale-mark-size+
                                         +black+))))
          (get-mouse-position :vec mouse-position)

          ;; Check whether to be scaling and drawing the drag indicator mark
          (or mouse-scale-mode
              (if (check-collision-point-rec mouse-position drag-rec)
                  (setf mouse-scale-ready t
                        mouse-scale-mode (is-mouse-button-pressed-p +mouse-button-left+))
                  (setf mouse-scale-ready nil)))

          ;; Perform scaling
          (when mouse-scale-mode
            (setf mouse-scale-ready t)

            ;; Scale the recs
            (let ((w (- (x mouse-position) (x rec)))
                  (h (- (y mouse-position) (y rec)))
                  (max-w (- (get-screen-width) (x rec)))
                  (max-h (- (get-screen-height) (y rec))))
              (dolist (rec (list rec mark-outline))
                (setf (width rec) (max (min w max-w) +mouse-scale-mark-size+)
                      (height rec) (max (min h max-h) +mouse-scale-mark-size+))))

            ;; Adjust drag rec
            (setf (x drag-rec) (drag-rec-component :x rec)
                  (y drag-rec) (drag-rec-component :y rec))

            ;; Update mark
            (let ((x (+ (x rec) (width rec)))
                  (y (+ (y rec) (height rec))))
              (with-accessors ((x1 x1) (y1 y1) (x2 x2) (y2 y2) (x3 x3) (y3 y3)) mark
                (setf x1 (- x +mouse-scale-mark-size+) y1 y
                      x2 x                             y2 y
                      x3 x                             y3 (- y +mouse-scale-mark-size+))))

            (when (is-mouse-button-released-p +mouse-button-left+)
              (setf mouse-scale-mode nil)))

          (with-drawing ()
            (draw-scene-except *scene* '(mark-outline mark))
            (when mouse-scale-ready (draw-scene *scene* '(mark-outline mark)))))))))
