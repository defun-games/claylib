(in-package #:claylib/wrap)

(cl:unless (uiop:getenv "CLAYLIB_USE_SYSTEM_RAYLIB_LIBRARIES")
  (cl:pushnew (uiop:unix-namestring
               (uiop:with-current-directory ((asdf:system-source-directory :claylib/wrap))
                 (uiop:merge-pathnames* "wrap/lib/")))
              cffi:*foreign-library-directories* :test #'cl:equal))

(cffi:define-foreign-library libraylib
  (:unix "libraylib.so")
  (t (:default "libraylib")))

(cffi:use-foreign-library libraylib)

(cffi:define-foreign-library libraygui
  (:unix "libraygui.so")
  (t (:default "libraygui")))

(cffi:use-foreign-library libraygui)

(cffi:define-foreign-library librayshim
  ((:and :x86-64 :unix) "librayshim.x86_64-pc-linux-gnu.so")
  (t (:default "librayshim")))

(cffi:use-foreign-library librayshim)

;;; Workaround for CLITERAL colors not being detected correctly by claw
(cl:defmacro setcolor (name cl:&rest coords)
  `(cl:progn
     (cl:defvar ,name (cffi:foreign-alloc 'color))
     ,(cl:append (cl:list 'cl:progn)
	         (cl:loop for k in '(r g b a)
		    for v in coords
                    collect `(cl:setf (cffi:foreign-slot-value ,name 'color ',k) ,v)))))

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

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(+lightgray+ +gray+ +darkgray+ +yellow+ +gold+ +orange+ +pink+ +red+ +maroon+
               +green+ +lime+ +darkgreen+ +skyblue+ +blue+ +darkblue+ +purple+ +violet+
               +darkpurple+ +beige+ +brown+ +darkbrown+ +white+ +black+ +blank+ +magenta+ +raywhite+)))
