(in-package #:cl-user)
(defpackage claylib/examples/gui-scroll-panel
  (:use :cl :claylib)
  (:export :main))
(in-package #:claylib/examples/gui-scroll-panel)

(defparameter *recs* (make-hash-table))

(defparameter *scene*
  (make-scene () ((text (make-text "[99.0, -20.0]" 4 4 :size 20 :color +red+))
                  (rec (make-rectangle 119 20 340 340 (fade +red+ 0.1 t))))))

(defun initialize-recs ()
  (setf (gethash 'group-box-1 *recs*) (make-simple-rec 550 170 220 205)
        (gethash 'label1 *recs*) (make-simple-rec 555 195 110 10)
        (gethash 'spinner1 *recs*) (make-simple-rec 670 190 90 20)
        (gethash 'label2 *recs*) (make-simple-rec 555 220 110 10)
        (gethash 'spinner2 *recs*) (make-simple-rec 670 215 90 20)
        (gethash 'label3 *recs*) (make-simple-rec 555 245 110 10)
        (gethash 'spinner3 *recs*) (make-simple-rec 670 240 90 20)
        (gethash 'checkbox *recs*) (make-simple-rec 565 280 20 20)
        (gethash 'label4 *recs*) (make-simple-rec 555 325 110 10)
        (gethash 'spinner4 *recs*) (make-simple-rec 670 320 90 20)
        (gethash 'label5 *recs*) (make-simple-rec 555 350 110 10)
        (gethash 'spinner5 *recs*) (make-simple-rec 670 345 90 20)
        (gethash 'toggle *recs*) (make-simple-rec 560 110 200 35)
        (gethash 'group-box-2 *recs*) (make-simple-rec 550 20 220 135)
        (gethash 'label6 *recs*) (make-simple-rec 555 35 110 10)
        (gethash 'spinner6 *recs*) (make-simple-rec 670 30 90 20)
        (gethash 'label7 *recs*) (make-simple-rec 555 60 110 10)
        (gethash 'spinner7 *recs*) (make-simple-rec 670 55 90 20)
        (gethash 'content-area *recs*) (make-simple-rec 565 80 20 20)
        (gethash 'content-width *recs*) (make-simple-rec 590 385 145 15)
        (gethash 'content-height *recs*) (make-simple-rec 590 410 145 15)))

(defun draw-control-set (control property text label-rec spinner-rec min max)
  (let ((style (gui-get-style control property)))
    (gui-label (gethash label-rec *recs*) text)
    (gui-spinner (gethash spinner-rec *recs*) "" style min max nil)
    (gui-set-style control property style)))

(defun draw-style-edit-controls ()
  (gui-group-box (gethash 'group-box-1 *recs*) "SCROLLBAR STYLE")
  (draw-control-set +scrollbar+ +border-width+ "BORDER_WIDTH" 'label1 'spinner1 0 6)
  (draw-control-set +scrollbar+ +arrows-size+ "ARROWS_SIZE" 'label2 'spinner2 4 14)
  (draw-control-set +scrollbar+ +slider-padding+ "SLIDER_PADDING" 'label3 'spinner3 0 14)

  (let ((style (gui-checkbox (gethash 'checkbox *recs*)
                             "ARROWS_VISIBLE"
                             (if (= 0 (gui-get-style +scrollbar+ +arrows-visible+))
                                 nil
                                 t))))
    (gui-set-style +scrollbar+ +arrows-visible+ (cond
                                                  ((eql style nil) 0)
                                                  ((eql style t) 1)
                                                  (t style))))

  (draw-control-set +scrollbar+ +slider-padding+ "SLIDER_PADDING" 'label4 'spinner4 0 14)
  (draw-control-set +scrollbar+ +slider-width+ "SLIDER_WIDTH" 'label5 'spinner5 2 100)

  (let* ((text (if (= (gui-get-style +listview+ +scrollbar-side+) +scrollbar-left-side+)
                   "SCROLLBAR: LEFT"
                   "SCROLLBAR: RIGHT"))
         (active (if (= (gui-get-style +listview+ +scrollbar-side+) 1)
                     t
                     nil))
         (style (gui-toggle (gethash 'toggle *recs*)
                            text
                            active)))
    (gui-set-style +listview+ +scrollbar-side+ (cond
                                                 ((eql style nil) 0)
                                                 ((eql style t) 1)
                                                 (t style))))

  (gui-group-box (gethash 'group-box-2 *recs*) "SCROLLPANEL STYLE")
  (draw-control-set +listview+ +scrollbar-width+ "SCROLLBAR_WIDTH" 'label6 'spinner6 6 30)
  (draw-control-set +default+ +border-width+ "BORDER_WIDTH" 'label7 'spinner7 0 20))

(defun main ()
  (with-window (:title "raygui - GuiScrollPanel()")
    (initialize-recs)
    (let ((panel-rec (make-simple-rec 20 40 200 150))
          (panel-content-rec (make-simple-rec 0 0 340 340))
          (panel-scroll (make-vector2 99 -20))
          (show-content-area t)
          (view (make-simple-rec 0 0 0 0)))
      (with-scenes *scene* ()
        (do-game-loop (:livesupport t)
          ;; TODO: Update logic???
          (with-drawing ()
            (draw-scene *scene* 'text)
            (gui-scroll-panel panel-rec "scroll panel" panel-content-rec panel-scroll :rec view)
            (with-scissor-mode
                (truncate (x view)) (truncate (y view))
                (truncate (width view)) (truncate (height view))
              (gui-grid (scene-object *scene* 'rec) "grid" 16.0 3))
            (when show-content-area
              (draw-scene *scene* 'rec))
            (draw-style-edit-controls)
            (let ((w (truncate (width panel-content-rec)))
                  (h (truncate (height panel-content-rec))))
              (setf show-content-area (gui-checkbox (gethash 'content-area *recs*)
                                                    "SHOW CONTENT AREA"
                                                    show-content-area)
                    (width panel-content-rec) (gui-slider-bar (gethash 'content-width *recs*)
                                                              "WIDTH"
                                                              (write-to-string w)
                                                              (width panel-content-rec)
                                                              1
                                                              600)
                    (height panel-content-rec) (gui-slider-bar (gethash 'content-height *recs*)
                                                               "HEIGHT"
                                                               (write-to-string h)
                                                               (height panel-content-rec)
                                                               1
                                                               400)))))))))
