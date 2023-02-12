(in-package #:cl-user)
(defpackage claylib/examples/input-box
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/input-box)

(defconstant +max-input-chars+ 9)

(defun append-codepoint (text-object code)
  "Appends the character given by the codepoint CODE to the TEXT-OBJECT's %TEXT."
  (with-accessors ((txt text)) text-object
    (setf txt (concatenate 'string txt (string (code-char code))))))

(defparameter *scene*
  (let ((box-x (- (/ *screen-width* 2) 100))
        (box-y 180)
        (box-w 225)
        (box-h 50))
    (make-scene ()
                ((instruct  (make-text "PLACE MOUSE OVER INPUT BOX!" 240 140 :size 20))
                 (box       (make-rectangle box-x box-y box-w box-h +lightgray+))
                 (box-line  (make-rectangle box-x box-y box-w box-h +darkgray+ :filled nil))
                 (txt-input (make-text "" (+ box-x 5) (+ box-y 8) :size 40 :color +maroon+))
                 (txt-chars (make-text "INPUT CHARS 0/9" 315 250 :size 20 :color +darkgray+))
                 (cursor    (make-text "_" (+ box-x 8) (+ box-y 12) :size 40 :color +maroon+))
                 (txt-del   (make-text "Press BACKSPACE to delete chars..." 230 300 :size 20))))))

(defun main ()
  (with-window (:title "raylib [text] example - input box")
    (with-scenes *scene* ()
      (with-scene-objects (box box-line cursor txt-input txt-chars) *scene*
        (do-game-loop (:livesupport t
                       :vars ((mouse-on-text-p nil)
                              (mouse-pos (make-vector2 0 0))
                              (frames-counter 0)
                              (text-changed-p nil)
                              (txt-input-size (make-vector2 0 0))
                              (letter-count 0)))
          (setf mouse-on-text-p (check-collision-point-rec (get-mouse-position :vec mouse-pos) box))

          (when mouse-on-text-p
            ;; Handle input
            (loop for key = (get-char-pressed) then (get-char-pressed)
                  until (<= key 0)
                  do (when (and (<= 32 key 125) (< letter-count +max-input-chars+))
                       (append-codepoint txt-input key)
                       (incf letter-count))
                  finally (setf text-changed-p t))
            (when (is-key-pressed-p +key-backspace+)
              (with-accessors ((txt text)) txt-input
                (setf letter-count (max 0 (1- letter-count))
                      txt (subseq txt 0 letter-count)
                      text-changed-p t)))

            ;; Update values
            (set-mouse-cursor +mouse-cursor-ibeam+)
            (setf frames-counter (1+ frames-counter)
                  (color box-line) +red+)
            (when text-changed-p
              (measure-text-ex txt-input :vector txt-input-size)
              (setf (text txt-chars) (format nil "INPUT CHARS ~d/9" letter-count)
                    (x cursor) (+ (x txt-input-size) (x box) 8))))

          (unless mouse-on-text-p
            ;; Update values
            (set-mouse-cursor +mouse-cursor-default+)
            (setf frames-counter 0
                  (color box-line) +darkgray+))

          (with-drawing ()
            (draw-scene-except *scene* '(cursor txt-del))
            (when mouse-on-text-p
              (draw-scene *scene*
                          (if (< letter-count +max-input-chars+)
                              (when (= (mod (truncate frames-counter 20) 2) 0)
                                'cursor)
                              'txt-del)))))))))
