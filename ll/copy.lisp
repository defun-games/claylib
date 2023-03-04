(in-package #:claylib/ll)

(defun copy-c-array (type src count &optional into)
  "Copy an entire C array. TYPE is an autowrapped type, SRC is the head of the source array, and COUNT
is the number of elements in the array. Pass INTO as the head of a dest array if desired."
  (let ((new (or into (cffi:foreign-funcall "calloc"
                                            :unsigned-int count
                                            :unsigned-int (cffi:foreign-type-size type)
                                            :void))))
    (unless (cffi:null-pointer-p src)
      (cffi:foreign-funcall "memcpy"
                            :pointer (cffi:mem-aptr new type)
                            :pointer (cffi:mem-aptr src type)
                            :int (* (cffi:foreign-type-size type) count)
                            :void))
    new))

(defun full-copy-shader (shader &optional into)
  (let ((new (or into (cffi:foreign-funcall "calloc"
                                            :unsigned-int 1
                                            :unsigned-int (cffi:foreign-type-size 'shader)
                                            :void)))
        (new-locs (copy-c-array :int
                                (cffi:foreign-slot-value shader 'shader 'locs)
                                32)))
    (setf (cffi:foreign-slot-value new 'shader 'id) (cffi:foreign-slot-value shader 'shader 'id)
          (cffi:foreign-slot-value new 'shader 'locs) (cffi:mem-aptr new-locs :int))
    new))

(defun full-copy-material (material &optional into)
  (let ((new (or into (cffi:foreign-funcall "calloc"
                                            :unsigned-int 1
                                            :unsigned-int (cffi:foreign-type-size 'material)
                                            :void)))
        (new-maps (copy-c-array 'material-map
                                (cffi:foreign-slot-value material 'material 'maps)
                                11)))
    (full-copy-shader (cffi:foreign-slot-value material 'material 'shader)
                      (cffi:foreign-slot-value new 'material 'shader))
    (setf (cffi:foreign-slot-value new 'material 'maps)
          (cffi:mem-aptr new-maps 'material-map))
    (copy-c-array :float
                  (cffi:foreign-slot-value material 'material 'params)
                  4
                  (cffi:foreign-slot-value new 'material 'params))
    new))

#|
(defgeneric full-copy (obj &optional into)
  (:documentation "Return a full copy of OBJ with all of its subfields and child structs copied as well.
Optional INTO if you already have an object to put the data in."))

(defmethod full-copy ((obj autowrap:wrapper) &optional into)
  ;; The default method works for anything that doesn't contain a pointer,
  ;; or contain any struct that itself contains a pointer.
  (let* ((type (type-of obj))
         (new (or into (autowrap:calloc type))))
    (autowrap:memcpy new obj :type type)
    new))

(defmethod full-copy ((obj image) &optional into)
  (image-copy (or into (autowrap:calloc 'image)) obj))

(defmethod full-copy ((obj glyph-info) &optional into)
  (let ((new (or into (autowrap:calloc 'glyph-info))))
    (setf (glyph-info.value new) (glyph-info.value obj)
          (glyph-info.offset-x new) (glyph-info.offset-x obj)
          (glyph-info.offset-y new) (glyph-info.offset-y obj)
          (glyph-info.advance-x new) (glyph-info.advance-x obj))
    (full-copy (glyph-info.image obj) (glyph-info.image new))
    new))

(defmethod full-copy ((obj font) &optional into)
  (let* ((num (font.glyph-count obj))
         (new (or into (autowrap:calloc 'font)))
         (new-recs (autowrap:calloc 'rectangle num))
         (new-glyphs (autowrap:calloc 'glyph-info num)))
    (full-copy (font.texture obj) (font.texture new))
    (copy-c-array 'rectangle (font.recs obj) num new-recs)
    (copy-c-array 'glyph-info (font.glyphs obj) num new-glyphs)
    (setf (font.base-size new) (font.base-size obj)
          (font.glyph-count new) (font.glyph-count obj)
          (font.glyph-padding new) (font.glyph-padding obj)
          (font.recs new) (autowrap:c-aptr new-recs 0 'rectangle)
          (font.glyphs new) (autowrap:c-aptr new-glyphs 0 'glyph-info))
    new))

(defmethod full-copy ((obj mesh) &optional into)
  (let* ((new (or into (autowrap:calloc 'mesh)))
         (vcount (mesh.vertex-count obj))
         (new-verts (copy-c-array :float (mesh.vertices obj) (* 3 vcount)))
         (new-tc (copy-c-array :float (mesh.texcoords obj) (* 2 vcount)))
         (new-tc2 (copy-c-array :float (mesh.texcoords2 obj) (* 2 vcount)))
         (new-norms (copy-c-array :float (mesh.normals obj) (* 3 vcount)))
         (new-tans (copy-c-array :float (mesh.tangents obj) (* 4 vcount)))
         (new-colors (copy-c-array :unsigned-char (mesh.colors obj) (* 4 vcount)))
         (new-inds (copy-c-array :unsigned-short (mesh.indices obj) vcount))
         (new-animv (copy-c-array :float (mesh.anim-vertices obj) (* 3 vcount)))
         (new-animn (copy-c-array :float (mesh.anim-normals obj) (* 3 vcount)))
         (new-boneids (copy-c-array :unsigned-char (mesh.bone-ids obj) (* 4 vcount)))
         (new-bonew (copy-c-array :float (mesh.bone-weights obj) (* 4 vcount)))
         (new-vboid (copy-c-array :unsigned-int (mesh.vbo-id obj) 7)))
    (setf (mesh.vertex-count new) vcount
          (mesh.triangle-count new) (mesh.triangle-count obj)
          (mesh.vao-id new) (mesh.vao-id obj)
          (mesh.vertices new) (autowrap:c-aptr new-verts 0 :float)
          (mesh.texcoords new) (autowrap:c-aptr new-tc 0 :float)
          (mesh.texcoords2 new) (autowrap:c-aptr new-tc2 0 :float)
          (mesh.normals new) (autowrap:c-aptr new-norms 0 :float)
          (mesh.tangents new) (autowrap:c-aptr new-tans 0 :float)
          (mesh.colors new) (autowrap:c-aptr new-colors 0 :unsigned-char)
          (mesh.indices new) (autowrap:c-aptr new-inds 0 :unsigned-short)
          (mesh.anim-vertices new) (autowrap:c-aptr new-animv 0 :float)
          (mesh.anim-normals new) (autowrap:c-aptr new-animn 0 :float)
          (mesh.bone-ids new) (autowrap:c-aptr new-boneids 0 :unsigned-char)
          (mesh.bone-weights new) (autowrap:c-aptr new-bonew 0 :float)
          (mesh.vbo-id new) (autowrap:c-aptr new-vboid 0 :unsigned-int))
    new))

(defmethod full-copy ((obj shader) &optional into)
  (let ((new (or into (autowrap:calloc 'shader)))
        (new-locs (copy-c-array :int (shader.locs obj) 32)))
    (setf (shader.id new) (shader.id obj)
          (shader.locs new) (autowrap:c-aptr new-locs 0 :int))
    new))

(defmethod full-copy ((obj material) &optional into)
  (let ((new (or into (autowrap:calloc 'material)))
        (new-maps (copy-c-array 'material-map (material.maps obj) 11)))
    (full-copy (material.shader obj) (material.shader new))
    (setf (material.maps new) (autowrap:c-aptr new-maps 0 'material-map))
    (dotimes (i 4)
      (setf (c-ref new material :params i)
            (c-ref obj material :params i)))
    new))

(defmethod full-copy ((obj model) &optional into)
  (let* ((new (or into (autowrap:calloc 'model)))
         (mesh-count (model.mesh-count obj))
         (mat-count (model.material-count obj))
         (bone-count (model.bone-count obj))
         (new-meshes (copy-c-array 'mesh (model.meshes obj) mesh-count))
         (new-mats (copy-c-array 'material (model.materials obj) mat-count))
         (new-mm (copy-c-array :int (model.mesh-material obj) mesh-count))
         (new-bones (copy-c-array 'bone-info (model.bones obj) bone-count))
         (new-bpose (copy-c-array 'transform (model.bind-pose obj) bone-count)))
    (full-copy (model.transform obj) (model.transform new))
    (setf (model.mesh-count new) mesh-count
          (model.material-count new) mat-count
          (model.meshes new) (autowrap:c-aptr new-meshes 0 'mesh)
          (model.materials new) (autowrap:c-aptr new-mats 0 'material)
          (model.mesh-material new) (autowrap:c-aptr new-mm 0 :int)
          (model.bone-count new) bone-count
          (model.bones new) (autowrap:c-aptr new-bones 0 'bone-info)
          (model.bind-pose new) (autowrap:c-aptr new-bpose 0 'transform))
    new))

(defmethod full-copy ((obj model-animation) &optional into)
  ;; TODO: Untested and I'm not sure the frame-poses field will work.
  ;; OTOH, I'm also not sure this method really has a use case.
  (let* ((new (or into (autowrap:calloc 'model-animation)))
         (bcount (model-animation.bone-count obj))
         (fcount (model-animation.frame-count obj))
         (new-bones (copy-c-array 'bone-info (model-animation.bones obj) bcount))
         (new-fposes (copy-c-array :pointer (model-animation.frame-poses obj) fcount)))
    (setf (model-animation.bone-count new) bcount
          (model-animation.frame-count new) fcount
          (model-animation.bones new) (autowrap:c-aptr new-bones 0 'bone-info)
          (model-animation.frame-poses new) (autowrap:c-aptr new-fposes 0 :pointer))
    new))

(defmethod full-copy ((obj wave) &optional into)
  (wave-copy (or into (autowrap:calloc 'wave)) obj))
|#

#|
(defgeneric partial-copy (obj copy-fields reuse-fields &optional into)
  (:documentation "Create a new OBJ. Copy the fields listed in COPY-FIELDS, reuse what's in REUSE-FIELDS.
Fields not included in either list are ignored! Optional INTO if you have a dest object already."))
|#

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
    `(defmethod ,(alexandria:symbolicate "partial-copy-" type)
         (obj copy-fields reuse-fields &optional into)
       (let ((,new (or into (cffi:foreign-funcall "calloc"
                                                  :unsigned-int 1
                                                  :unsigned-int ,(cffi:foreign-type-size type)
                                                  :void)))
             ,@(loop for field in field-defs
                     with n = -1
                     when (and (third field)
                               (not (fourth field)))
                       do (incf n)
                       and collect `(,(elt new-arrays n)
                                     (when (member ',(first field) copy-fields)
                                       (copy-c-array ,(second field)
                                                     (,(alexandria:symbolicate type
                                                                               "."
                                                                               (first field))
                                                      obj)
                                                     ,(third field))))))
         ,@(loop for field in field-defs
                 with n = -1
                 collect (destructuring-bind (fname ftype &optional count fixed) field
                           (let ((fun (alexandria:symbolicate type
                                                              "."
                                                              fname)))
                             (cond
                               ;; Fixed-length array
                               ((and count fixed)
                                `(cond
                                   ((member ',fname copy-fields)
                                    (copy-c-array ,ftype
                                                  (cffi:foreign-slot-value obj ',type ',fname)
                                                  ,count
                                                  (cffi:foreign-slot-value ,new ',type ',fname)))
                                   ((member ',fname reuse-fields)
                                    ,(if (member ftype (append cffi:*built-in-float-types*
                                                               cffi:*built-in-foreign-types*
                                                               cffi:*built-in-integer-types*
                                                               cffi:*other-builtin-types*))
                                         `(error "Reuse of fixed array type ~A not supported for field ~A. You must copy or ignore."
                                                 ,ftype ',fname)
                                         `(copy-c-array ,ftype
                                                        (cffi:foreign-slot-value obj ',type ',fname)
                                                        ,count
                                                        (cffi:foreign-slot-value ,new ',type ',fname))))))
                               
                               ;; Variable-length array
                               (count
                                (incf n)
                                `(cond
                                   ((member ',fname copy-fields)
                                    (setf (,fun ,new) (cffi:mem-aptr ,(elt new-arrays n) ,ftype)))
                                   ((member ',fname reuse-fields)
                                    (setf (,fun ,new) (,fun obj)))))
                               
                               ;; CFFI type
                               ((member ftype (append cffi:*built-in-float-types*
                                                      cffi:*built-in-foreign-types*
                                                      cffi:*built-in-integer-types*
                                                      cffi:*other-builtin-types*))
                                `(cond
                                   ((member ',fname copy-fields)
                                    (setf (,fun ,new) (,fun obj)))
                                   ((member ',fname reuse-fields)
                                    (error "Reuse of builtin type ~A not supported for field ~A. You must copy or ignore."
                                           ,ftype ',fname))))
                               
                               ;; Wrapper type
                               (t
                                `(cond
                                   ((member ',fname copy-fields)
                                    (full-copy (,fun obj) (,fun ,new)))
                                   ((member ',fname reuse-fields)
                                    (error "Reuse of wrapper type ~A not supported for field ~A. You must copy or ignore."
                                           ,ftype ',fname))))))))
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
                (recs 'rectangle (font.base-size obj))
                (glyphs 'glyph-info (font.base-size obj))))

(defpcopy camera3d ((position 'vector3)
                    (target 'vector3)
                    (up 'vector3)
                    (fovy :float)
                    (projection :int)))

(defpcopy camera2d ((offset 'vector2) (target 'vector2) (rotation :float) (zoom :float)))

(defpcopy mesh ((vertex-count :int)
                (triangle-count :int)
                (vertices :float (* 3 (mesh.vertex-count obj)))
                (texcoords :float (* 2 (mesh.vertex-count obj)))
                (texcoords2 :float (* 2 (mesh.vertex-count obj)))
                (normals :float (* 3 (mesh.vertex-count obj)))
                (tangents :float (* 4 (mesh.vertex-count obj)))
                (colors :unsigned-char (* 4 (mesh.vertex-count obj)))
                (indices :unsigned-short (mesh.vertex-count obj))
                (anim-vertices :float (* 3 (mesh.vertex-count obj)))
                (anim-normals :float (* 3 (mesh.vertex-count obj)))
                (bone-ids :unsigned-char (* 4 (mesh.vertex-count obj)))
                (bone-weights :float (* 4 (mesh.vertex-count obj)))
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
                 (meshes 'mesh (model.mesh-count obj))
                 (materials 'material (model.material-count obj))
                 (mesh-material :int (model.mesh-count obj))
                 (bone-count :int)
                 (bones 'bone-info (model.bone-count obj))
                 (bind-pose 'transform (model.bone-count obj))))

(defpcopy model-animation ((bone-count :int)
                           (frame-count :int)
                           (bones 'bone-info (model-animation.bone-count obj))
                           (frame-poses :pointer (model-animation.frame-count obj))))

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
