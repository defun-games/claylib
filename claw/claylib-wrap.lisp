(in-package #:claylib/wrap)

(cl:unless (uiop:getenv "CLAYLIB_USE_SYSTEM_RAYLIB_LIBRARIES")
  (cl:pushnew (uiop:unix-namestring
               (uiop:with-current-directory ((asdf:system-source-directory :claylib/wrap))
                 (uiop:merge-pathnames* "claw/lib/")))
              cffi:*foreign-library-directories* :test #'cl:equal))

(cffi:define-foreign-library libraylib
  (:unix "libraylib.so")
  (t (:default "libraylib")))

(cffi:use-foreign-library libraylib)

(cffi:define-foreign-library libraygui
  (:unix "libraygui.so")
  (t (:default "libraygui")))

(cffi:use-foreign-library libraygui)
