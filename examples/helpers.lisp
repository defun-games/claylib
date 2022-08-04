(in-package #:claylib/examples)

(defun claylib-path (path)
  "Return a pathname from the PATH relative to the claylib project root. Mostly just for use in the
claylib examples."
  (asdf:system-relative-pathname :claylib path))
