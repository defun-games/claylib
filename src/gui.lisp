(in-package #:claylib)

;;;; Raygui Lispification
;;;; Obligatory disclaimer: Everything in this file is in flux and will probably get moved.
;;;; I don't fully know what direction this will go yet.



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
  (bounds rl-rectangle))

(defun-pt gui-scroll-panel claylib/ll:gui-scroll-panel
  "Scroll Panel control. Allocates a new RL-RECTANGLE unless you pass one."
  (rec rl-rectangle nil (make-instance 'rl-rectangle))
  (bounds rl-rectangle)
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
  (text string))

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
