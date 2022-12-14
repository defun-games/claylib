(in-package #:claylib/wrap)

(dolist (dir '(#p"./lib/"
               #p"./lib-patched/"))
  (pushnew dir cffi:*foreign-library-directories* :test #'equal))

(pushnew (uiop:unix-namestring
          (uiop:with-current-directory ((asdf:system-source-directory :claylib/wrap))
            (uiop:merge-pathnames* "wrap/lib/")))
         cffi:*foreign-library-directories* :test #'equal)

(cffi:define-foreign-library libraylib
  (:unix "libraylib.so")
  (t (:default "libraylib")))

(cffi:use-foreign-library libraylib)

(cffi:define-foreign-library libraygui
  (:unix "libraygui.so")
  (t (:default "libraygui")))

(cffi:use-foreign-library libraygui)

(autowrap:c-include '(claylib/wrap wrap lib "raygui.h")
                    :release-p t
                    :spec-path '(claylib/wrap wrap spec)
                    ;; NOTE: We are currently excluding features where autowrap is having difficulties.
                    :exclude-definitions (;; stdarg.h: only needed for TraceLogCallback
                                          "va_list" "__gnuc_va_list"
                                          ;; colors: not detected correctly and set to NIL
                                          "LIGHTGRAY" "GRAY" "DARKGRAY" "YELLOW" "GOLD" "ORANGE" "PINK"
                                          "RED" "MAROON" "GREEN" "LIME" "DARKGREEN" "SKYBLUE" "BLUE"
                                          "DARKBLUE" "PURPLE" "VIOLET" "DARKPURPLE" "BEIGE" "BROWN"
                                          "DARKBROWN" "WHITE" "BLACK" "BLANK" "MAGENTA" "RAYWHITE")
                    ;; Some function names collide with CL
                    :symbol-exceptions (("remove" . "C-REMOVE")
                                        ("random" . "C-RANDOM")
                                        ("abort" . "C-ABORT")
                                        ("abs" . "C-ABS")
                                        ("acos" . "C-ACOS")
                                        ("asin" . "C-ASIN")
                                        ("atan" . "C-ATAN")
                                        ("cos" . "C-COS")
                                        ("sin" . "C-SIN")
                                        ("tan" . "C-TAN")
                                        ("cosh" . "C-COSH")
                                        ("sinh" . "C-SINH")
                                        ("tanh" . "C-TANH")
                                        ("acosh" . "C-ACOSH")
                                        ("asinh" . "C-ASINH")
                                        ("atanh" . "C-ATANH")
                                        ("exp" . "C-EXP")
                                        ("log" . "C-LOG")
                                        ("sqrt" . "C-SQRT")
                                        ("floor" . "C-FLOOR")
                                        ("round" . "C-ROUND")
                                        ;; Some of them just goofed.
                                        ("GuiCheckBox" . "GUI-CHECKBOX")
                                        ("PROGRESSBAR" . "+PROGRESS-BAR+")
                                        ("COMBOBOX" . "+COMBO-BOX+")
                                        ("DROPDOWNBOX" . "+DROPDOWN-BOX+")
                                        ("TEXTBOX" . "+TEXT-BOX+")
                                        ("VALUEBOX" . "+VALUE-BOX+")
                                        ("LISTVIEW" . "+LIST-VIEW+")
                                        ("COLORPICKER" . "+COLOR-PICKER+")
                                        ("STATUSBAR" . "+STATUS-BAR+")))

(autowrap:c-include '(claylib/wrap wrap lib "raymath.h")
                    :release-p t
                    :spec-path '(claylib/wrap wrap spec)
                    ;; Duplicate definitions from above
                    :exclude-definitions ("^Vector2$" "^Vector3$" "^Vector4$" "^Matrix$")
                    ;; Some function names collide with CL
                    :symbol-exceptions (("acos" . "C-ACOS")
                                        ("asin" . "C-ASIN")
                                        ("atan" . "C-ATAN")
                                        ("cos" . "C-COS")
                                        ("sin" . "C-SIN")
                                        ("tan" . "C-TAN")
                                        ("cosh" . "C-COSH")
                                        ("sinh" . "C-SINH")
                                        ("tanh" . "C-TANH")
                                        ("acosh" . "C-ACOSH")
                                        ("asinh" . "C-ASINH")
                                        ("atanh" . "C-ATANH")
                                        ("exp" . "C-EXP")
                                        ("log" . "C-LOG")
                                        ("sqrt" . "C-SQRT")
                                        ("floor" . "C-FLOOR")
                                        ("round" . "C-ROUND"))
                    ;; Some names just aren't pretty enough
                    :symbol-regex (("Vector2[A-Z]" ()
                                                      (lambda (string matches regex)
                                                        (ppcre:regex-replace "Vector2(?!$)"
                                                                             string
                                                                             "Vector2-")))
                                   ("Vector3[A-Z]" ()
                                                      (lambda (string matches regex)
                                                        (ppcre:regex-replace "Vector3(?!$)"
                                                                             string
                                                                             "Vector3-")))))

;;; Workaround for the color issue described above
(defmacro setcolor (name &rest coords)
  `(progn
     (defvar ,name (autowrap:alloc 'color))
     ,(append (list 'progn)
	      (loop for k in '(:r :g :b :a)
		    for v in coords
                    collect `(setf (plus-c:c-ref ,name color ,k) ,v)))))

(setcolor +lightgray+ 200 200 200 255)
(setcolor +gray+ 130 130 130 255)
(setcolor +darkgray+ 80 80 80 255)
(setcolor +yellow+ 253 249 0 255)
(setcolor +gold+ 255 203 0 255)
(setcolor +orange+ 255 161 0 255)
(setcolor +pink+ 255 109 194 255)
(setcolor +red+ 230 41 55 255)
(setcolor +maroon+ 190 33 55 255)
(setcolor +green+ 0 228 48 255)
(setcolor +lime+ 0 158 47 255)
(setcolor +darkgreen+ 0 117 44 255)
(setcolor +skyblue+ 102 191 255 255)
(setcolor +blue+ 0 121 241 255)
(setcolor +darkblue+ 0 82 172 255)
(setcolor +purple+ 200 122 255 255)
(setcolor +violet+ 135 60 190 255)
(setcolor +darkpurple+ 112 31 126 255)
(setcolor +beige+ 211 176 131 255)
(setcolor +brown+ 127 106 79 255)
(setcolor +darkbrown+ 76 63 47 255)
(setcolor +white+ 255 255 255 255)
(setcolor +black+ 0 0 0 255)
(setcolor +blank+ 0 0 0 0)
(setcolor +magenta+ 255 0 255 255)
(setcolor +raywhite+ 245 245 245 255)
