(in-package #:claylib)

;;;; Raygui Lispification
;;;; Obligatory disclaimer: Everything in this file is in flux and will probably get moved.
;;;; I don't fully know what direction this will go yet.



;;; Inheritable classes

(defclass gui-object ()
  ((%bounds :initarg :bounds
            :type rl-rectangle
            :accessor bounds)))

(defclass text-label ()
  ((%text :initarg :text
          :type string
          :accessor text)))

(defclass text-box ()
  ((%text-size :initarg :text-size
               :type integer
               :accessor text-size)))

(defclass pressable ()
  ((%pressed
    :initform nil
    :type boolean
    :reader pressed
    :documentation "The 'pressed' slot is a return value of the drawing function. It rarely makes
sense to set this in your own code.")))

(defclass editable ()
  ((%edit-mode :initarg :edit-mode
               :type boolean
               :accessor edit-mode)))

(defclass int-values ()
  ((%value :initarg :value
           :type integer  ; TODO: pointer
           :accessor value)
   (%min-value :initarg :min-value
               :type integer
               :accessor min-value)
   (%max-value :initarg :max-value
               :type integer
               :accessor max-value)))

(defclass value-bar ()
  ((%text-left :initarg :text-left
               :type string
               :accessor text-left)
   (%text-right :initarg :text-right
                :type string
                :accessor text-right)
   (%value :initarg :value
           :type number
           :reader value)
   (%min-value :initarg :min-value
               :type number
               :reader min-value)
   (%max-value :initarg :max-value
               :type number
               :reader max-value)))
(defwriter-float value value-bar)
(defwriter-float min-value value-bar)
(defwriter-float max-value value-bar)

(defclass gui-color ()
  ((%color :initarg :color
           :type rl-color
           :accessor color)))



;;; Container/separator controls

(defclass gui-window-box (gui-object pressable)
  ((%title :initarg :title
           :type string
           :accessor title))
  (:documentation "Window Box control, shows a window that can be closed"))
(defmethod draw-object ((obj gui-window-box))
  (setf (slot-value obj '%pressed)
        (gui-window-box (bounds obj) (title obj))))

(defclass gui-group-box (gui-object text-label) ()
  (:documentation "Group Box control with text name"))
(defmethod draw-object ((obj gui-group-box))
  (gui-group-box (bounds obj) (text obj)))

(defclass gui-line (gui-object text-label) ()
  (:documentation "Line separator control, could contain text"))
(defmethod draw-object ((obj gui-line))
  (gui-line (bounds obj) (text obj)))

(defclass gui-panel (gui-object text-label) ()
  (:documentation "Panel control, useful to group controls"))
(defmethod draw-object ((obj gui-panel))
  (gui-panel (bounds obj) (text obj)))

(defclass gui-scroll-panel (gui-object text-label)
  ((%content :initarg :content
             :type rl-rectangle
             :accessor content)
   (%scroll :initarg :scroll
            :type rl-vector2  ; TODO: pointer
            :accessor scroll)
   (%view
    :initform (make-simple-rec 0 0 1 1)
    :type rl-rectangle
    :reader view
    :documentation "The GUI-SCROLL-PANEL view rectangle is a libffi return value only. It rarely makes sense to
set this in your own code."))
  (:documentation "Scroll Panel control"))
(defmethod draw-object ((obj gui-scroll-panel))
  (gui-scroll-panel (bounds obj)
                    (text obj)
                    (content obj)
                    (scroll obj)
                    :rec (view obj)))



;;; Basic controls set

(defclass gui-label (gui-object text-label) ()
  (:documentation "Label control, shows text"))
(defmethod draw-object ((obj gui-label))
  (gui-label (bounds obj) (text obj)))

(defclass gui-button (gui-object text-label pressable) ()
  (:documentation "Button control, sets PRESSED when clicked"))
(defmethod draw-object ((obj gui-button))
  (setf (slot-value obj '%pressed)
        (gui-button (bounds obj) (text obj))))

(defclass gui-label-button (gui-object text-label pressable) ()
  (:documentation "Label Button control, sets PRESSED when clicked"))
(defmethod draw-object ((obj gui-label-button))
  (setf (slot-value obj '%pressed)
        (gui-label-button (bounds obj) (text obj))))

(defclass gui-toggle (gui-object text-label)
  ((%active :initarg :active
            :type boolean
            :accessor active))
  (:documentation "Toggle Button control, sets ACTIVE when active"))
(defmethod draw-object ((obj gui-toggle))
  (setf (active obj)
        (gui-toggle (bounds obj) (text obj) (active obj))))

(defclass gui-toggle-group (gui-object text-label)
  ((%active :initarg :active
            :type integer
            :accessor active))
  (:documentation "Toggle Group control, sets ACTIVE to active toggle index"))
(defmethod draw-object ((obj gui-toggle-group))
  (setf (active obj)
        (gui-toggle-group (bounds obj) (text obj) (active obj))))

(defclass gui-checkbox (gui-object text-label)
  ((%checked :initarg :checked
             :type boolean
             :accessor checked))
  (:documentation "Checkbox control, sets CHECKED when active"))
(defmethod draw-object ((obj gui-checkbox))
  (setf (checked obj)
        (gui-checkbox (bounds obj) (text obj) (checked obj))))

(defclass gui-combo-box (gui-object text-label)
  ((%active :initarg :active
            :type integer
            :accessor active))
  (:documentation "Combo Box control, sets ACTIVE to selected item index"))
(defmethod draw-object ((obj gui-combo-box))
  (setf (active obj)
        (gui-combo-box (bounds obj) (text obj) (active obj))))

(defclass gui-dropdown-box (gui-object text-label pressable editable)
  ((%active :initarg :active
            :type integer  ; TODO: pointer
            :accessor active))
  (:documentation "Dropdown Box control, sets PRESSED when clicked"))
(defmethod draw-object ((obj gui-dropdown-box))
  (setf (slot-value obj '%pressed)
        (gui-dropdown-box (bounds obj)
                          (text obj)
                          (active obj)
                          (edit-mode obj))))

(defclass gui-spinner (gui-object text-label pressable editable int-values) ()
  (:documentation "Spinner control, sets PRESSED when clicked"))
(defmethod draw-object ((obj gui-spinner))
  (setf (slot-value obj '%pressed)
        (gui-spinner (bounds obj)
                     (text obj)
                     (value obj)
                     (min-value obj)
                     (max-value obj)
                     (edit-mode obj))))

(defclass gui-value-box (gui-spinner) ()
  (:documentation "Value Box control, sets PRESSED when clicked"))
(defmethod draw-object ((obj gui-value-box))
  (setf (slot-value obj '%pressed)
        (gui-value-box (bounds obj)
                       (text obj)
                       (value obj)
                       (min-value obj)
                       (max-value obj)
                       (edit-mode obj))))

(defclass gui-text-box (gui-object text-label text-box pressable editable) ()
  (:documentation "Text Box control, updates input text, sets PRESSED when entered"))
(defmethod draw-object ((obj gui-text-box))
  (setf (slot-value obj '%pressed)
        (gui-text-box (bounds obj)
                      (text obj)
                      (text-size obj)
                      (edit-mode obj))))

(defclass gui-text-box-multi (gui-text-box) ()
  (:documentation "Text Box control with multiple lines"))
(defmethod draw-object ((obj gui-text-box-multi))
  (setf (slot-value obj '%pressed)
        (gui-text-box-multi (bounds obj)
                            (text obj)
                            (text-size obj)
                            (edit-mode obj))))

(defclass gui-slider (gui-object value-bar) ()
  (:documentation "Slider control, sets VALUE to selected value"))
(defmethod draw-object ((obj gui-slider))
  (setf (value obj)
        (gui-slider (bounds obj)
                    (text-left obj)
                    (text-right obj)
                    (value obj)
                    (min-value obj)
                    (max-value obj))))

(defclass gui-slider-bar (gui-object value-bar) ()
  (:documentation "Slider Bar control, sets VALUE to selected value"))
(defmethod draw-object ((obj gui-slider-bar))
  (setf (value obj)
        (gui-slider-bar (bounds obj)
                        (text-left obj)
                        (text-right obj)
                        (value obj)
                        (min-value obj)
                        (max-value obj))))

(defclass gui-progress-bar (gui-object value-bar) ()
  (:documentation "Progress Bar control, sets VALUE to current progress value"))
(defmethod draw-object ((obj gui-progress-bar))
  (setf (value obj)
        (gui-progress-bar (bounds obj)
                          (text-left obj)
                          (text-right obj)
                          (value obj)
                          (min-value obj)
                          (max-value obj))))

(defclass gui-status-bar (gui-object text-label) ()
  (:documentation "Status Bar control, shows info text"))
(defmethod draw-object ((obj gui-status-bar))
  (gui-status-bar (bounds obj) (text obj)))

(defclass gui-dummy-rec (gui-object text-label) ()
  (:documentation "Dummy control for placeholders"))
(defmethod draw-object ((obj gui-dummy-rec))
  (gui-dummy-rec (bounds obj) (text obj)))

(defclass gui-grid (gui-object text-label)
  ((%spacing :initarg :spacing
             :type number
             :reader spacing)
   (%subdivs :initarg :subdivs
             :type integer
             :accessor subdivs)
   (%position
    :initform (make-vector2 0 0)
    :type rl-vector2
    :reader pos
    :documentation "The GUI-GRID position vector is a libffi return value only. It rarely makes sense to
set this in your own code."))
  (:documentation "Grid control, sets POSITION to mouse cell position"))
(defwriter-float spacing gui-grid)
(defmethod draw-object ((obj gui-grid))
  (gui-grid (bounds obj)
            (text obj)
            (spacing obj)
            (subdivs obj)
            :vec (pos obj)))



;;; Advance controls set

(defclass gui-list-view (gui-object text-label)
  ((%scroll-index :initarg :scroll-index
                  :type integer  ; TODO: pointer
                  :accessor scroll-index)
   (%active :initarg :active
            :type integer
            :accessor active)
   (%num :initarg :num
         :type integer
         :accessor num)
   (%focus :initarg :focus
           :type integer  ; TODO: pointer
           :accessor focus))
  (:documentation "List View control, sets ACTIVE to selected list item index"))
(defmethod draw-object ((obj gui-list-view))
  (setf (active obj)
        (gui-list-view (bounds obj)
                       (text obj)
                       (scroll-index obj)
                       (active obj)
                       :count (when (slot-boundp obj '%num) (num obj))
                       :focus (when (slot-boundp obj '%focus) (focus obj)))))

(defclass gui-message-box (gui-object)
  ((%title :initarg :title
           :type string
           :accessor title)
   (%message :initarg :message
             :type string
             :accessor message)
   (%buttons :initarg :buttons
             :type string
             :accessor buttons)
   (%selected
    :initform -1
    :type integer
    :reader selected
    :documentation "The 'selected' slot is a return value of the drawing function. It rarely makes
sense to set this in your own code."))
  (:documentation "Message Box control, displays a message, sets SELECTED to clicked button from list"))
(defmethod draw-object ((obj gui-message-box))
  (setf (slot-value obj '%selected)
        (gui-message-box (bounds obj)
                         (title obj)
                         (message obj)
                         (buttons obj))))

(defclass gui-text-input-box (gui-message-box text-label)
  ((%text-max-size :initarg :text-max-size
                   :type integer
                   :accessor text-max-size)
   (%secret-view-active :initarg :secret-view-active
                        :type integer  ; TODO: pointer
                        :accessor secret-view-active))
  (:documentation "Text Input Box control, ask for text, supports secret"))
(defmethod draw-object ((obj gui-text-input-box))
  (setf (slot-value obj '%selected)
        (gui-text-input-box (bounds obj)
                            (title obj)
                            (message obj)
                            (buttons obj)
                            (text obj)
                            (text-max-size obj)
                            (secret-view-active obj))))

(defclass gui-color-picker (gui-object text-label gui-color) ()
  (:documentation "Color Picker control (multiple color controls), sets COLOR to chosen color"))
(defmethod draw-object ((obj gui-color-picker))
  (gui-color-picker (bounds obj)
                    (color obj)
                    :col (color obj)))

(defclass gui-color-panel (gui-object text-label gui-color) ()
  (:documentation "Color Panel control, sets COLOR to chosen color"))
(defmethod draw-object ((obj gui-color-panel))
  (gui-color-panel (bounds obj)
                   (color obj)
                   :col (color obj)))

(defclass gui-color-bar-alpha (gui-object text-label)
  ((%alpha :initarg :alpha
           :type number
           :reader alpha))
  (:documentation "Color Bar Alpha control, sets ALPHA to alpha value"))
(defwriter-float alpha gui-color-bar-alpha)
(defmethod draw-object ((obj gui-color-bar-alpha))
  (setf (alpha obj)
        (gui-color-bar-alpha (bounds obj) (alpha obj))))

(defclass gui-color-bar-hue (gui-object text-label)
  ((%value :initarg :value
           :type number
           :reader value))
  (:documentation "Color Bar Hue control, sets VALUE to hue value"))
(defwriter-float value gui-color-bar-hue)
(defmethod draw-object ((obj gui-color-bar-hue))
  (setf (value obj)
        (gui-color-bar-hue (bounds obj) (value obj))))



;;; Font set/get functions

(defun-pt-void gui-set-font claylib/ll:gui-set-font
  "Set gui custom font (global state)"
  (font rl-font))

(defun-pt gui-get-font claylib/ll:gui-get-font
  "Get gui custom font (global state). Allocates a new RL-FONT unless you pass one."
  (font rl-font nil (make-instance 'rl-font)))



;;; Container/separator controls

(defun-pt-bool gui-window-box claylib/ll:gui-window-box
  "Window Box control, shows a window that can be closed"
  (bounds rl-rectangle)
  (title string))

(defun-pt-void gui-group-box claylib/ll:gui-group-box
  "Group Box control with text name"
  (bounds rl-rectangle)
  (text string))

(defun-pt-void gui-line claylib/ll:gui-line
  "Line separator control, could contain text"
  (bounds rl-rectangle)
  (text string))

(defun-pt-void gui-panel claylib/ll:gui-panel
  "Panel control, useful to group controls"
  (bounds rl-rectangle)
  (text string))

(defun-pt gui-scroll-panel claylib/ll:gui-scroll-panel
  "Scroll Panel control. Allocates a new RL-RECTANGLE unless you pass one."
  (rec rl-rectangle nil (make-instance 'rl-rectangle))
  (bounds rl-rectangle)
  (text string)
  (content rl-rectangle)
  (scroll rl-vector2))



;; Basic controls set

(defun-pt-void gui-label claylib/ll:gui-label
  "Label control, shows text"
  (bounds rl-rectangle)
  (text string))

(defun-pt-bool gui-button claylib/ll:gui-button
  "Button control, returns T when clicked"
  (bounds rl-rectangle)
  (text string))

(defun-pt-bool gui-label-button claylib/ll:gui-label-button
  "Label button control, returns T when clicked"
  (bounds rl-rectangle)
  (text string))

(defun-pt-bool gui-toggle claylib/ll:gui-toggle
  "Toggle Button control, returns T when active"
  (bounds rl-rectangle)
  (text string)
  (active boolean))

(defun-pt-num gui-toggle-group claylib/ll:gui-toggle-group
  "Toggle Group control, returns active toggle index"
  (bounds rl-rectangle)
  (text string)
  (active integer))

(defun-pt-bool gui-checkbox claylib/ll:gui-checkbox
  "Checkbox control, returns T when active"
  (bounds rl-rectangle)
  (text string)
  (checked boolean))

(defun-pt-num gui-combo-box claylib/ll:gui-combo-box
  "Combo Box control, returns selected item index"
  (bounds rl-rectangle)
  (text string)
  (active integer))

(defun-pt-bool gui-dropdown-box claylib/ll:gui-dropdown-box
  "Dropdown Box control, returns selected item"
  (bounds rl-rectangle)
  (text string)
  (active integer)
  (edit-mode boolean))

(defun-pt-bool gui-spinner claylib/ll:gui-spinner
  "Spinner control, returns selected value"
  (bounds rl-rectangle)
  (text string)
  (value integer)
  (min-value integer)
  (max-value integer)
  (edit-mode boolean))

(defun gui-spinner (bounds text value min-value max-value edit-mode)
  "Spinner control, returns selected value"
  (check-type bounds rl-rectangle)
  (check-type text string)
  (check-type value integer)
  (check-type min-value integer)
  (check-type max-value integer)
  (check-type edit-mode boolean)
  (c-with ((val :int :value value))
    (if (= 0 (claylib/wrap:gui-spinner (c-struct bounds)
                                       text
                                       (val &)
                                       min-value
                                       max-value
                                       (if edit-mode 1 0)))
        nil
        t)))

(defun-pt-bool gui-value-box claylib/ll:gui-value-box
  "Value Box control, updates input text with numbers"
  (bounds rl-rectangle)
  (text string)
  (value integer)
  (min-value integer)
  (max-value integer)
  (edit-mode boolean))

(defun-pt-bool gui-text-box claylib/ll:gui-text-box
  "Text Box control, updates input text"
  (bounds rl-rectangle)
  (text string)
  (text-size integer)
  (edit-mode boolean))

(defun-pt-bool gui-text-box-multi claylib/ll:gui-text-box-multi
  "Text Box control with multiple lines"
  (bounds rl-rectangle)
  (text string)
  (text-size integer)
  (edit-mode boolean))

(defun-pt-num gui-slider claylib/ll:gui-slider
  "Slider control, returns selected value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defun-pt-num gui-slider-bar claylib/ll:gui-slider-bar
  "Slider Bar control, returns selected value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defun-pt-num gui-progress-bar claylib/ll:gui-progress-bar
  "Progress Bar control, shows current progress value"
  (bounds rl-rectangle)
  (text-left string)
  (text-right string)
  (value number float)
  (min-value number float)
  (max-value number float))

(defun-pt-void gui-status-bar claylib/ll:gui-status-bar
  "Status Bar control, shows info text"
  (bounds rl-rectangle)
  (text string))

(defun-pt-void gui-dummy-rec claylib/ll:gui-dummy-rec
  "Dummy control for placeholders"
  (bounds rl-rectangle)
  (text string))

(defun-pt-num gui-scroll-bar claylib/ll:gui-scroll-bar
  "Scroll Bar control"
  (bounds rl-rectangle)
  (value integer)
  (min-value integer)
  (max-value integer))

(defun-pt gui-grid claylib/ll:gui-grid
  "Grid control. Allocates a new RL-VECTOR2 unless you pass one."
  (vec rl-vector2 nil (make-vector2 0 0))
  (bounds rl-rectangle)
  (text string)
  (spacing number float)
  (subdivs integer))



;;; Advance controls set

(defun gui-list-view (bounds text scroll-index active &key count focus)
  "List View control, returns selected list item index"
  (check-type bounds rl-rectangle)
  (check-type text string)
  (check-type scroll-index integer)
  (check-type active integer)
  (check-type count (or null integer))
  (check-type focus (or null integer))
  (if (and count focus)
      (claylib/ll:gui-list-view-ex (c-struct bounds) text count focus scroll-index active)
      (claylib/ll:gui-list-view (c-struct bounds) text scroll-index active)))

(defun-pt-num gui-message-box claylib/ll:gui-message-box
  "Message Box control, displays a message"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string))

(defun-pt-num gui-text-input-box claylib/ll:gui-text-input-box
  "Text Input Box control, ask for text"
  (bounds rl-rectangle)
  (title string)
  (message string)
  (buttons string)
  (text string)
  (text-max-size integer)
  (secret-view-active integer))

(defun-pt gui-color-picker claylib/ll:gui-color-picker
  "Color Picker control (multiple color controls). Allocates a new RL-COLOR unless you pass one."
  (col rl-color nil (make-color 0 0 0))
  (bounds rl-rectangle)
  (color rl-color))

(defun-pt gui-color-panel claylib/ll:gui-color-panel
  "Color Panel control. Allocates a new RL-COLOR unless you pass one."
  (col rl-color nil (make-color 0 0 0))
  (bounds rl-rectangle)
  (color rl-color))

(defun-pt-num gui-color-bar-alpha claylib/ll:gui-color-bar-alpha
  "Color Bar Alpha control"
  (bounds rl-rectangle)
  (alpha number float))

(defun-pt-num gui-color-bar-hue claylib/ll:gui-color-bar-hue
  "Color Bar Hue control"
  (bounds rl-rectangle)
  (value number float))



;;; Icons functionality

(defun-pt-void gui-draw-icon claylib/ll:gui-draw-icon
  "Draw a gui icon"
  (icon-id integer)
  (pos-x integer)
  (pos-y integer)
  (pixel-size integer)
  (color rl-color))

(defun-pt-bool gui-check-icon-pixel claylib/ll:gui-check-icon-pixel
  "Check icon pixel value"
  (icon-id integer)
  (x integer)
  (y integer))
