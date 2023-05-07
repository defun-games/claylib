(in-package #:claylib/ll)

(defun copy-c-array (type src count &optional into)
  "Copy an entire C array. TYPE is an autowrapped type, SRC is the head of the source array, and COUNT
is the number of elements in the array. Pass INTO as the head of a dest array if desired."
  (let ((new (or into (calloc type count))))
    (unless (cffi:null-pointer-p src)
      (cffi:foreign-funcall "memcpy"
                            :pointer (cffi:mem-aptr new type)
                            :pointer (cffi:mem-aptr src type)
                            :int (* (cffi:foreign-type-size type) count)
                            :void))
    new))

(defun full-copy (obj type &optional into)
  ;; The default function works for anything that doesn't contain a pointer,
  ;; or contain any struct that itself contains a pointer.
  (let ((new (or into (calloc type))))
    (cffi:foreign-funcall "memcpy"
                          :pointer new
                          :pointer obj
                          :int (cffi:foreign-type-size type)
                          :void)
    new))

(defun full-copy-image (image &optional into)
  (image-copy (or into (calloc 'image)) image))

(defun full-copy-glyph-info (glyph-info &optional into)
  (let ((new (or into (calloc 'glyph-info))))
    (dolist (field '(value offset-x offset-y advance-x))
      (setf (field-value new 'glyph-info field)
            (field-value glyph-info 'glyph-info field)))
    (full-copy-image (field-ptr glyph-info 'glyph-info 'image)
                     (field-ptr new 'glyph-info 'image))))

(defun full-copy-font (font &optional into)
  (let* ((num (field-value font 'font 'glyph-count))
         (new (or into (calloc 'font)))
         (new-recs (calloc 'rectangle num))
         (new-glyphs (calloc 'glyph-info num)))
    (full-copy (field-ptr font 'font 'texture) 'texture (field-ptr new 'font 'texture))
    (copy-c-array 'rectangle (field-ptr font 'font 'recs) num new-recs)
    (copy-c-array 'glyph-info (field-ptr font 'font 'glyphs) num new-glyphs)
    (dolist (field '(base-size glyph-count glyph-padding recs glyphs))
      (setf (field-value new 'font field)
            (field-value font 'font 'field)))
    new))

(defun full-copy-mesh (mesh &optional into)
  (let* ((new (or into (calloc 'mesh)))
         (vcount (field-value mesh 'mesh 'vertex-count))
         (new-verts (copy-c-array :float (field-ptr mesh 'mesh 'vertices) (* 3 vcount)))
         (new-tc (copy-c-array :float (field-ptr mesh 'mesh 'texcoords) (* 2 vcount)))
         (new-tc2 (copy-c-array :float (field-ptr mesh 'mesh 'texcoords2) (* 2 vcount)))
         (new-norms (copy-c-array :float (field-ptr mesh 'mesh 'normals) (* 3 vcount)))
         (new-tans (copy-c-array :float (field-ptr mesh 'mesh 'tangents) (* 4 vcount)))
         (new-colors (copy-c-array :unsigned-char (field-ptr mesh 'mesh 'colors) (* 4 vcount)))
         (new-inds (copy-c-array :unsigned-short (field-ptr mesh 'mesh 'indices) vcount))
         (new-animv (copy-c-array :float (field-ptr mesh 'mesh 'anim-vertices) (* 3 vcount)))
         (new-animn (copy-c-array :float (field-ptr mesh 'mesh 'anim-normals) (* 3 vcount)))
         (new-boneids (copy-c-array :unsigned-char (field-ptr mesh 'mesh 'bone-ids) (* 4 vcount)))
         (new-bonew (copy-c-array :float (field-ptr mesh 'mesh 'bone-weights) (* 4 vcount)))
         (new-vboid (copy-c-array :unsigned-int (field-ptr mesh 'mesh 'vbo-id) 7)))
    (dolist (field '(vertex-count triangle-count vao-id))
      (setf (field-value new 'mesh field)
            (field-value mesh 'mesh field)))
    (loop for field in '(vertices texcoords texcoords2 normals tangents colors indices anim-vertices
                         anim-normals bone-ids bone-weights vbo-id)
          for array in (list new-verts new-tc new-tc2 new-norms new-tans new-colors new-inds
                             new-animv new-animn new-boneids new-bonew new-vboid)
          for type in '(:float :float :float :float :float :unsigned-char :unsigned-short :float
                        :float :unsigned-char :float :unsigned-int)
          do (setf (field-value new 'mesh field)
                   (cffi:mem-aptr array type)))
    new))

(defun full-copy-shader (shader &optional into)
  (let ((new (or into (calloc 'shader)))
        (new-locs (copy-c-array :int
                                (field-value shader 'shader 'locs)
                                32)))
    (setf (field-value new 'shader 'id) (field-value shader 'shader 'id)
          (field-value new 'shader 'locs) (cffi:mem-aptr new-locs :int))
    new))

(defun full-copy-material (material &optional into)
  (let ((new (or into (calloc 'material)))
        (new-maps (copy-c-array 'material-map
                                (field-value material 'material 'maps)
                                11)))
    (full-copy-shader (field-ptr material 'material 'shader)
                      (field-ptr new 'material 'shader))
    (setf (field-value new 'material 'maps)
          (cffi:mem-aptr new-maps 'material-map))
    (copy-c-array :float
                  (field-value material 'material 'params)
                  4
                  (field-value new 'material 'params))
    new))

(defun full-copy-model (model &optional into)
  (let* ((new (or into (calloc 'model)))
         (mesh-count (field-value model 'model 'mesh-count))
         (mat-count (field-value model 'model 'material-count))
         (bone-count (field-value model 'model 'bone-count))
         (new-meshes (copy-c-array 'mesh (field-ptr model 'model 'meshes) mesh-count))
         (new-mats (copy-c-array 'material (field-ptr model 'model 'materials) mat-count))
         (new-mm (copy-c-array :int (field-ptr model 'model 'mesh-material) mesh-count))
         (new-bones (copy-c-array 'bone-info (field-ptr model 'model 'bones) bone-count))
         (new-bpose (copy-c-array 'transform (field-ptr model 'model 'bind-pose) bone-count)))
    (full-copy (field-ptr model 'model 'transform) 'matrix (field-ptr new 'model new))
    (loop for field in '(mesh-count material-count meshes materials mesh-material bone-count
                         bones bind-pose)
          for value in (list mesh-count
                             mat-count
                             (cffi:mem-aptr new-meshes 'mesh)
                             (cffi:mem-aptr new-mats 'material)
                             (cffi:mem-aptr new-mm :int)
                             bone-count
                             (cffi:mem-aptr new-bones 'bone-info)
                             (cffi:mem-aptr new-bpose 'transform))
          do (setf (field-value new 'model field) value))
    new))

(defun full-copy-model-animation (anim &optional into)
  ;; TODO: Untested and I'm not sure the frame-poses field will work.
  ;; OTOH, I'm also not sure this function really has a use case.
  (let* ((new (or into (calloc 'model-animation)))
         (bcount (field-value anim 'model-animation 'bone-count))
         (fcount (field-value anim 'model-animation 'frame-count))
         (new-bones (copy-c-array 'bone-info (field-ptr anim 'model-animation 'bones) bcount))
         (new-fposes (copy-c-array :pointer (field-ptr anim 'model-animation 'frame-poses) fcount)))
    (setf (field-value new 'model-animation 'bone-count) bcount
          (field-value new 'model-animation 'frame-count) fcount
          (field-value new 'model-animation 'bones) (cffi:mem-aptr new-bones 'bone-info)
          (field-value new 'model-animation 'frame-poses) (cffi:mem-aptr new-fposes :pointer))
    new))

(defun full-copy-wave (wave &optional into)
  (wave-copy (or into (calloc 'wave)) wave))

(defun full-copy-vector2 (vec &optional into)
  (full-copy vec 'vector2 into))
(defun full-copy-vector3 (vec &optional into)
  (full-copy vec 'vector3 into))
(defun full-copy-vector4 (vec &optional into)
  (full-copy vec 'vector4 into))
(defun full-copy-matrix (matrix &optional into)
  (full-copy matrix 'matrix into))
(defun full-copy-color (color &optional into)
  (full-copy color 'color into))
(defun full-copy-rectangle (rect &optional into)
  (full-copy rect 'rectangle into))
(defun full-copy-texture (tex &optional into)
  (full-copy tex 'texture into))
(defun full-copy-render-texture (tex &optional into)
  (full-copy tex 'render-texture into))
(defun full-copy-n-patch-info (np &optional into)
  (full-copy np 'n-patch-info into))
(defun full-copy-camera-3d (camera &optional into)
  (full-copy camera 'camera-3d into))
(defun full-copy-camera-2d (camera &optional into)
  (full-copy camera 'camera-2d into))
(defun full-copy-material-map (matmap &optional into)
  (full-copy matmap 'material-map into))
(defun full-copy-transform (trans &optional into)
  (full-copy trans 'transform into))
(defun full-copy-bone-info (bone &optional into)
  (full-copy bone 'boneinfo into))
(defun full-copy-ray (ray &optional into)
  (full-copy ray 'ray into))
(defun full-copy-ray-collision (rc &optional into)
  (full-copy rc 'ray-collision into))
(defun full-copy-bounding-box (box &optional into)
  (full-copy box 'bounding-box into))
(defun full-copy-vr-device-info (info &optional into)
  (full-copy info 'vr-device-info into))
(defun full-copy-vr-stereo-config (config &optional into)
  (full-copy config 'vr-stereo-config into))



(defmacro defpcopy (type field-defs)
  "Create a PARTIAL-COPY method for a wrapper type. FIELD-DEFS is a list containing of the form:

(FIELD-NAME FIELD-TYPE &optional COUNT FIXED)

COUNT is an array size, if applicable. Pass :FIXED T if the array is fixed-length. A valid FIELD-TYPE is
any CFFI type or wrapper type."
  (let ((new (gensym))
        (new-arrays (alexandria:make-gensym-list
                     (loop for field in field-defs
                           count (and (third field)
                                      (not (fourth field)))))))
    `(defun ,(alexandria:symbolicate "PARTIAL-COPY-" type)
         (obj copy-fields reuse-fields &optional into)
       (let ((,new (or into (calloc ',type)))
             ,@(loop for field in field-defs
                     with n = -1
                     when (and (third field)
                               (not (fourth field)))
                       do (incf n)
                       and collect `(,(elt new-arrays n)
                                     (when (member ',(first field) copy-fields)
                                       (copy-c-array ,(second field)
                                                     (field-value obj
                                                                  ',(alexandria:symbolicate type)
                                                                  ',(alexandria:symbolicate (first field)))
                                                     ,(third field))))))
         ,@(loop for field in field-defs
                 with n = -1
                 collect (destructuring-bind (fname ftype &optional count fixed) field
                           (cond
                             ;; Fixed-length array
                             ((and count fixed)
                              `(cond
                                 ((member ',fname copy-fields)
                                  (copy-c-array ,ftype
                                                (field-value obj ',type ',fname)
                                                ,count
                                                (field-value ,new ',type ',fname)))
                                 ((member ',fname reuse-fields)
                                  ,(if (builtin-type-p ftype)
                                       `(error "Reuse of fixed array type ~A not supported for field ~A. You must copy or ignore."
                                               ,ftype ',fname)
                                       `(copy-c-array ,ftype
                                                      (field-value obj ',type ',fname)
                                                      ,count
                                                      (field-value ,new ',type ',fname))))))

                             ;; Variable-length array
                             (count
                              (incf n)
                              `(cond
                                 ((member ',fname copy-fields)
                                  (setf (field-value ,new ',type ',fname)
                                        (cffi:mem-aptr ,(elt new-arrays n) ,ftype)))
                                 ((member ',fname reuse-fields)
                                  (setf (field-value ,new ',type ',fname)
                                        (field-value obj ',type ',fname)))))

                             ;; CFFI type
                             ((builtin-type-p ftype)
                              `(cond
                                 ((member ',fname copy-fields)
                                  (setf (field-value ,new ',type ',fname)
                                        (field-value obj ',type ',fname)))
                                 ((member ',fname reuse-fields)
                                  (error "Reuse of builtin type ~A not supported for field ~A. You must copy or ignore."
                                         ,ftype ',fname))))

                             ;; Wrapper types
                             (t
                              `(cond
                                 ((member ',fname copy-fields)
                                  (apply #',(alexandria:symbolicate "FULL-COPY-"
                                                                    (string-left-trim
                                                                     '(#\')
                                                                     (format nil "~A" ftype)))
                                         (field-value obj ',type ',fname)
                                         (list (field-value ,new ',type ',fname))))
                                 ((member ',fname reuse-fields)
                                  (error "Reuse of wrapper type ~A not supported for field ~A. You must copy or ignore."
                                         ,ftype ',fname)))))))
         ,new))))

(defpcopy vector2 ((x :float) (y :float)))

(defpcopy vector3 ((x :float) (y :float) (z :float)))

(defpcopy vector4 ((x :float) (y :float) (z :float) (w :float)))

(defpcopy matrix ((m0 :float) (m4 :float) (m8 :float) (m12 :float)
                  (m1 :float) (m5 :float) (m9 :float) (m13 :float)
                  (m2 :float) (m6 :float) (m10 :float) (m14 :float)
                  (m3 :float) (m7 :float) (m11 :float) (m15 :float)))

(defpcopy color ((r :unsigned-char) (g :unsigned-char) (b :unsigned-char) (a :unsigned-char)))

(defpcopy rectangle ((x :float) (y :float) (width :float) (height :float)))

(defun partial-copy-image (image copy-fields reuse-fields &optional into)
  (declare (ignorable image copy-fields reuse-fields into))
  (error "PARTIAL-COPY not supported on IMAGE. Only FULL-COPY makes sense."))

(defpcopy texture ((id :unsigned-int) (width :int) (height :int) (mipmaps :int) (format :int)))

(defpcopy render-texture ((id :unsigned-int) (texture 'texture) (depth 'texture)))

(defpcopy n-patch-info ((source 'rectangle)
                        (left :int)
                        (top :int)
                        (right :int)
                        (bottom :int)
                        (layout :int)))

(defpcopy glyph-info ((value :int) (offset-x :int) (offset-y :int) (advance-x :int) (image 'image)))

(defpcopy font ((base-size :int)
                (glyph-count :int)
                (glyph-padding :int)
                (texture 'texture)
                (recs 'rectangle (field-value obj 'font 'base-size))
                (glyphs 'glyph-info (field-value obj 'font 'base-size))))

(defpcopy camera-3d ((position 'vector3)
                     (target 'vector3)
                     (up 'vector3)
                     (fovy :float)
                     (projection :int)))

(defpcopy camera-2d ((offset 'vector2) (target 'vector2) (rotation :float) (zoom :float)))

(defpcopy mesh ((vertex-count :int)
                (triangle-count :int)
                (vertices :float (* 3 (field-value obj 'mesh 'vertex-count)))
                (texcoords :float (* 2 (field-value obj 'mesh 'vertex-count)))
                (texcoords2 :float (* 2 (field-value obj 'mesh 'vertex-count)))
                (normals :float (* 3 (field-value obj 'mesh 'vertex-count)))
                (tangents :float (* 4 (field-value obj 'mesh 'vertex-count)))
                (colors :unsigned-char (* 4 (field-value obj 'mesh 'vertex-count)))
                (indices :unsigned-short (field-value obj 'mesh 'vertex-count))
                (anim-vertices :float (* 3 (field-value obj 'mesh 'vertex-count)))
                (anim-normals :float (* 3 (field-value obj 'mesh 'vertex-count)))
                (bone-ids :unsigned-char (* 4 (field-value obj 'mesh 'vertex-count)))
                (bone-weights :float (* 4 (field-value obj 'mesh 'vertex-count)))
                (vao-id :unsigned-int)
                (vbo-id :unsigned-int 7)))

(defpcopy shader ((id :unsigned-int) (locs :int 32)))

(defpcopy material-map ((texture 'texture) (color 'color) (value :float)))

(defpcopy material ((shader 'shader) (maps 'material-map 11) (params :float 4 t)))

(defpcopy transform ((translation 'vector3) (rotation 'vector4) (scale 'vector3)))

(defpcopy bone-info ((name :char 32 t) (parent :int)))

(defpcopy model ((transform 'matrix)
                 (mesh-count :int)
                 (material-count :int)
                 (meshes 'mesh (field-value obj 'model 'mesh-count))
                 (materials 'material (field-value obj 'model 'material-count))
                 (mesh-material :int (field-value obj 'model 'mesh-count))
                 (bone-count :int)
                 (bones 'bone-info (field-value obj 'model 'bone-count))
                 (bind-pose 'transform (field-value obj 'model 'bone-count))))

(defpcopy model-animation ((bone-count :int)
                           (frame-count :int)
                           (bones 'bone-info (field-value obj 'model-animation 'bone-count))
                           (frame-poses :pointer (field-value obj 'model-animation 'frame-count))))

(defpcopy ray ((position 'vector3) (direction 'vector3)))

(defpcopy ray-collision ((hit :bool) (distance :float) (point 'vector3) (normal 'vector3)))

(defpcopy bounding-box ((min 'vector3) (max 'vector3)))

(defun partial-copy-wave (wave copy-fields reuse-fields &optional into)
  (declare (ignorable wave copy-fields reuse-fields into))
  (error "PARTIAL-COPY not supported on WAVE. Only FULL-COPY makes sense."))

(defpcopy vr-device-info ((h-resolution :int)
                          (v-resolution :int)
                          (h-screen-size :float)
                          (v-screen-size :float)
                          (v-screen-center :float)
                          (eye-to-screen-distance :float)
                          (lens-separation-distance :float)
                          (interpupillary-distance :float)
                          (lens-distortion-values :float 4 t)
                          (chroma-ab-correction :float 4 t)))

(defpcopy vr-stereo-config ((left-lens-center :float 2 t)
                            (right-lens-center :float 2 t)
                            (left-screen-center :float 2 t)
                            (right-screen-center :float 2 t)
                            (scale :float 2 t)
                            (scale-in :float 2 t)))
