(defpackage #:claylib/ll
  (:use #:cl #:plus-c #:claylib/wrap)
  (:export

   ;;; Core

   ;; Window-related functions
   :init-window :window-should-close-p :close-window :is-window-ready-p :is-window-fullscreen-p
   :is-window-hidden-p :is-window-minimized-p :is-window-maximized-p :is-window-focused-p
   :is-window-resized-p :is-window-state-p :set-window-state :clear-window-state :toggle-fullscreen
   :maximize-window :minimize-window :restore-window :set-window-icon :set-window-title
   :set-window-position :set-window-min-size :set-window-size :get-window-handle :get-screen-width
   :get-screen-height :get-monitor-count :get-current-monitor :get-monitor-position :get-monitor-width
   :get-monitor-height :get-monitor-physical-width :get-monitor-physical-height :get-monitor-refresh-rate
   :get-window-position :get-window-scale-dpi :get-monitor-name :set-clipboard-text :get-clipboard-text

   ;; Cursor-related functions
   :show-cursor :hide-cursor :is-cursor-hidden-p :enable-cursor :disable-cursor :is-cursor-on-screen-p

   ;; Drawing-related functions
   :clear-background :begin-drawing :end-drawing :begin-mode2d :end-mode2d :begin-mode3d :end-mode3d
   :begin-texture-mode :end-texture-mode :begin-shader-mode :end-shader-mode :begin-blend-mode
   :end-blend-mode :begin-scissor-mode :end-scissor-mode :begin-vr-stereo-mode :end-vr-stereo-mode

   ;; VR stereo config functions for VR simulator
   :load-vr-stereo-config :unload-vr-stereo-config

   ;; Shader management functions
   :load-shader :load-shader-from-memory :get-shader-location :get-shader-location-attrib
   :set-shader-value :set-shader-value-v :set-shader-value-matrix :set-shader-value-texture
   :unload-shader

   ;; Screen-space-related functions
   :get-mouse-ray :get-camera-matrix :get-camera-matrix-2d :get-world-to-screen :get-world-to-screen-ex
   :get-world-to-screen2d :get-screen-to-world2d

   ;; Timing-related functions
   :set-target-fps :get-fps :get-frame-time :get-time

   ;; Misc. functions
   :get-random-value :set-random-seed :take-screenshot :set-config-flags :trage-log :set-trace-log
   :mem-alloc :mem-realloc :mem-free

   ;; Set custom callbacks
   :set-trace-log-callback :set-load-file-data-callback :set-save-file-data-callback
   :set-load-file-text-callback :set-save-file-text-callback

   ;; Files management functions
   :load-file-data :unload-file-data :save-file-data :load-file-text :unload-file-text :save-file-text
   :file-exists-p :directory-exists-p :is-file-extension-p :get-file-extension :get-file-name
   :get-file-name-without-ext :get-directory-path :get-prev-directory-path :get-working-directory
   :get-directory-files :clear-directory-files :change-directory :is-file-dropped-p :get-dropped-files
   :clear-dropped-files :get-file-mod-time

   ;; Compression/Encoding functionality
   :compress-data :decompress-data :encode-data-base64 :decode-data-base64

   ;; Persistent storage management
   :save-storage-value :load-storage-value

   ;; Misc.
   :open-url

   ;; Input-related functions: keyboard
   :is-key-pressed-p :is-key-down-p :is-key-released-p :is-key-up-p :set-exit-key :get-key-pressed
   :get-char-pressed

   ;; Input-related functions: gamepads
   :is-gamepad-available-p :get-gamepad-name :is-gamepad-button-pressed-p :is-gamepad-button-down-p
   :is-gamepad-button-released-p :is-gamepad-button-up-p :get-gamepad-button-pressed
   :get-gamepad-axis-count :get-gamepad-axis-movement :set-gamepad-mappings

   ;; Input-related functions: mouse
   :is-mouse-button-pressed-p :is-mouse-button-down-p :is-mouse-button-released-p :is-mouse-button-up-p
   :get-mouse-x :get-mouse-y :get-mouse-position :get-mouse-delta :set-mouse-position :set-mouse-offset
   :set-mouse-scale :get-mouse-wheel-move :set-mouse-cursor

   ;; Input-related functions: touch
   :get-touch-x :get-touch-y :get-touch-position :get-touch-point-id :get-touch-point-count

   ;; Gestures and Touch Handling Functions (Module: rgestures)
   :set-gestures-enabled :is-gesture-detected-p :get-gesture-detected :get-gesture-hold-duration
   :get-gesture-drag-vector :get-gesture-drag-angle :get-gesture-pinch-vector :get-gesture-pinch-angle

   ;; Camera System Functions (Module: rcamera)
   :set-camera-mode :update-camera :set-camera-pan-control :set-camera-alt-control
   :set-camera-smooth-zoom-control :set-camera-move-controls

   ;;; Shapes

   ;; Set texture and rectangle to be used on shapes drawing
   :set-shapes-texture

   ;; Basic shapes drawing functions
   :draw-pixel :draw-pixel-v :draw-line :draw-line-v :draw-line-ex :draw-line-bezier
   :draw-line-bezier-quad :draw-line-bezier-cubic :draw-line-strip :draw-circle :draw-circle-sector
   :draw-circle-sector-lines :draw-circle-gradient :draw-circle-v :draw-circle-lines :draw-ellipse
   :draw-ellipse-lines :draw-ring :draw-ring-lines :draw-rectangle :draw-rectangle-v :draw-rectangle-rec
   :draw-rectangle-pro :draw-rectangle-gradient-v :draw-rectangle-gradient-h :draw-rectangle-gradient-ex
   :draw-rectangle-lines :draw-rectangle-lines-ex :draw-rectangle-rounded :draw-rectangle-rounded-lines
   :draw-triangle :draw-triangle-lines :draw-triangle-fan :draw-triangle-strip :draw-poly
   :draw-poly-lines :draw-poly-lines-ex

   ;; Basic shapes collision detection functions
   :check-collision-recs :check-collision-circles :check-collision-circle-rec :check-collision-point-rec
   :check-collision-point-circle :check-collision-point-triangle :check-collision-lines
   :check-collision-point-line :get-collision-rec

   ;;; Textures

   ;; Image loading functions
   :load-image :load-image-raw :load-image-anim :load-image-from-memory :load-image-from-texture
   :load-image-from-screen :unload-image :export-image :export-image-as-code

   ;; Image generation functions
   :gen-image-color :gen-image-gradient :gen-image-gradient-v :gen-image-gradient-h
   :gen-image-gradient-radial :gen-image-checked :gen-image-white-noise :gen-image-cellular

   ;; Image manipulation functions
   :image-copy :image-from-image :image-text :image-text-ex :image-format :image-to-pot :image-crop
   :image-alpha-crop :image-alpha-clear :image-alpha-mask :image-alpha-premultiply :image-resize
   :image-resize-nn :image-resize-canvas :image-mipmaps :image-dither :image-flip-vertical
   :image-flip-horizontal :image-rotate-cw :image-rotate-ccw :image-color-tint :image-color-invert
   :image-color-grayscale :image-color-brightness :image-color-replace :load-image-colors
   :load-image-palette :unload-image-colors :unload-image-palette :get-image-alpha-border
   :get-image-color

   ;; Image drawing functions
   :image-clear-background :image-draw-pixel :image-draw-pixel-v :image-draw-line :image-draw-line-v
   :image-draw-circle :image-draw-circle-v :image-draw-rectangle :image-draw-rectangle-v
   :image-draw-rectangle-rec :image-draw-rectangle-lines :image-draw :image-draw-text
   :image-draw-text-ex

   ;; Texture loading functions
   :load-texture :load-texture-from-image :load-texture-cubemap :load-render-texture :unload-texture
   :unload-render-texture :update-texture :update-texture-rec

   ;; Texture configuration functions
   :gen-texture-mipmaps :set-texture-filter :set-texture-wrap

   ;; Texture drawing functions
   :draw-texture :draw-texture-v :draw-texture-ex :draw-texture-rec :draw-texture-quad
   :draw-texture-tiled :draw-texture-pro :draw-texture-n-patch :draw-texture-poly

   ;; Color/pixel related functions
   :fade :color-to-int :color-normalize :color-from-normalized :color-to-hsv :color-from-hsv
   :color-alpha :color-alpha-blend :get-color :get-pixel-color :set-pixel-color :get-pixel-data-size

   ;;; Text

   ;; Font loading/unloading functions
   :get-font-default :load-font :load-font-ex :load-font-from-image :load-font-from-memory
   :load-font-data :gen-image-font-atlas :unload-font-data :unload-font

   ;; Text-drawing functions
   :draw-fps :draw-text :draw-text-ex :draw-text-pro :draw-text-codepoint

   ;; Text misc. functions
   :measure-text :measure-text-ex :get-glyph-index :get-glyph-info :get-glyph-atlas-rec

   ;; Text codepoints management functions
   :load-codepoints :unload-codepoints :get-codepoint-count :get-codepoint :codepoint-to-utf8
   :text-codepoints-to-utf8

   ;; Text strings management functions
   :text-copy :text-is-equal-p :text-length :text-format :text-subtext :text-replace :text-insert
   :text-join :text-split :text-append :text-find-index :text-to-upper :text-to-lower :text-to-pascal
   :text-to-integer

   ;;; Models

   ;; Basic geometric 3D shapes drawing functions
   :draw-line-3d :draw-point-3d :draw-circle-3d :draw-triangle-3d :draw-triangle-strip-3d :draw-cube
   :draw-cube-v :draw-cube-wires :draw-cube-wires-v :draw-cube-texture :draw-cube-texture-rec
   :draw-sphere :draw-sphere-ex :draw-sphere-wires :draw-cylinder :draw-cylinder-ex :draw-cylinder-wires
   :draw-cylinder-wires-ex :draw-plane :draw-ray :draw-grid

   ;; Model loading/unloading functions
   :load-model :load-model-from-mesh :unload-model :unload-model-keep-meshes :get-model-bounding-box

   ;; Model drawing functions
   :draw-model :draw-model-ex :draw-model-wires :draw-model-wires-ex :draw-bounding-box
   :draw-billboard :draw-billboard-rec :draw-billboard-pro
   
   ;; Mesh management functions
   :upload-mesh :update-mesh-buffer :unload-mesh :draw-mesh :draw-mesh-instanced :export-mesh
   :get-mesh-bounding-box :gen-mesh-tangents :gen-mesh-binormals 

   ;; Mesh generation functions
   :gen-mesh-poly :gen-mesh-plane :gen-mesh-cube :gen-mesh-sphere :gen-mesh-hemisphere
   :gen-mesh-cylinder :gen-mesh-cone :gen-mesh-torus :gen-mesh-knot :gen-mesh-heightmap
   :gen-mesh-cubicmap

   ;; Material loading/unloading functions
   :load-materials :load-material-default :unload-material :set-material-texture :set-model-mesh-material

   ;; Model animations loading/unloading functions
   :load-model-animations :update-model-animation :unload-model-animation :unload-model-animations
   :is-model-animation-valid-p

   ;; Collision detection functions
   :check-collision-spheres :check-collision-boxes :check-collision-box-sphere :get-ray-collision-sphere
   :get-ray-collision-box :get-ray-collision-model :get-ray-collision-mesh :get-ray-collision-triangle
   :get-ray-collision-quad

   ;;; Audio

   ;; Audio device management functions
   :init-audio-device :close-audio-device :is-audio-device-ready-p :set-master-volume

   ;; Wave/Sound loading/unloading functions
   :load-wave :load-wave-from-memory :load-sound :load-sound-from-wave :update-sound :unload-wave
   :unload-sound :export-wave :export-wave-as-code

   ;; Wave/Sound management functions
   :play-sound :stop-sound :pause-sound :resume-sound :play-sound-multi :stop-sound-multi
   :get-sounds-playing :is-sound-playing-p :set-sound-volume :set-sound-pitch :wave-format :wave-copy
   :wave-crop :load-wave-samples :unload-wave-samples

   ;; Music management functions
   :load-music-stream :load-music-stream-from-memory :unload-music-stream :play-music-stream
   :is-music-stream-playing-p :update-music-stream :stop-music-stream :pause-music-stream
   :resume-music-stream :seek-music-stream :set-music-volume :set-music-pitch :get-music-time-length
   :get-music-time-played

   ;; AudioStream management functions
   :init-audio-stream :update-audio-stream :close-audio-stream :is-audio-stream-processed-p
   :play-audio-stream :pause-audio-stream :resume-audio-stream :is-audio-stream-playing-p
   :stop-audio-stream :set-audio-stream-volume :set-audio-stream-pitch
   :set-audio-stream-buffer-size-default

   ;;; Structs

   :vector2 :vector2.x :vector2.y
   :vector3 :vector3.x :vector3.y :vector3.z
   :vector4 :vector4.x :vector4.y :vector4.z :vector4.w :quaternion
   :matrix :matrix.m0 :matrix.m4 :matrix.m8 :matrix.m12 :matrix.m1 :matrix.m5 :matrix.m9 :matrix.m13
   :matrix.m2 :matrix.m6 :matrix.m10 :matrix.m14 :matrix.m3 :matrix.m7 :matrix.m11 :matrix.m15
   :color :color.r :color.g :color.b :color.a
   :rectangle :rectangle.x :rectangle.y :rectangle.width :rectangle.height

   :image :image.data :image.width :image.height :image.mipmaps :image.format
   :texture :texture.id :texture.width :texture.height :texture.mipmaps :texture.format :texture2d
   :texture-cubemap
   :render-texture :render-texture.id :render-texture.texture :render-texture.depth :render-texture2d
   :n-patch-info :n-patch-info.source :n-patch-info.left :n-patch-info.top :n-patch-info.right
   :n-patch-info.bottom :n-patch-info.layout
   :glyph-info :glyph-info.value :glyph-info.offset-x :glyph-info.offset-y :glyph-info.advance-x
   :glyph-info.image
   :font :font.base-size :font.glyph-count :font.glyph-padding :font.texture :font.recs :font.glyphs

   :camera3d :camera3d.position :camera3d.target :camera3d.up :camera3d.fovy :camera3d.projection
   :camera2d :camera2d.offset :camera2d.target :camera2d.rotation :camera2d.zoom
   :mesh :mesh.vertex-count :mesh.triangle-count :mesh.vertices :mesh.texcoords :mesh.texcoords2
   :mesh.normals :mesh.tangents :mesh.colors :mesh.indices :mesh.anim-vertices :mesh.anim-normals
   :mesh.bone-ids :mesh.bone-weights :mesh.vao-id :mesh.vbo-id
   :shader :shader.id :shader.locs
   :material-map :material-map.texture :material-map.color :material-map.value
   :material :material.shader :material.maps :material.params
   :model :model.transform :model.mesh-count :model.material-count :model.meshes :model.materials
   :model.mesh-material :model.bone-count :model.bones :model.bind-pose
   :transform :transform.translation :transform.rotation :transform.scale
   :bone-info :bone-info.name :bone-info.parent
   :model-animation :model-animation.bone-count :model-animation.frame-count :model-animation.bones
   :model-animation.frame-poses
   :ray :ray.position :ray.direction
   :ray-collision :ray-collision.hit :ray-collision.distance :ray-collision.point :ray-collision.normal
   :bounding-box :bounding-box.min :bounding-box.max

   :wave :wave.frame-count :wave.sample-rate :wave.sample-size :wave.channels :wave.data
   :sound :sound.stream :sound.frame-count
   :music :music.stream :music.frame-count :music.looping :music.ctx-type :music.ctx-data
   :audio-stream :audio-stream.buffer :audio-stream.sample-rate :audio-stream.sample-size
   :audio-stream.channels

   :vr-device-info :vr-device-info.h-resolution :vr-device-info.v-resolution
   :vr-device-info.h-screen-size :vr-device-info.v-screen-size :vr-device-info.v-screen-center
   :vr-device-info.eye-to-screen-distance :vr-device-info.lens-separation-distance
   :vr-device-info.interpupillary-distance :vr-device-info.lens-distortion-values
   :vr-device-info.chroma-ab-correction
   :vr-stereo-config :vr-stereo-config.projection :vr-stereo-config.view-offset
   :vr-stereo-config.left-lens-center :vr-stereo-config.right-lens-center
   :vr-stereo-config.left-screen-center :vr-stereo-config.right-screen-center :vr-stereo-config.scale
   :vr-stereo-config.scale-in
   
   ;;; Constants
   
   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+ :+green+
   :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+ :+beige+
   :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+

   ;; Blending
   :+blend-alpha+ :+blend-additive+ :+blend-multiplied+ :+blend-add-colors+ :+blend-subtract-colors+
   :+blend-custom+
   
   ;; Keys
   :+key-a+ :+key-b+ :+key-c+ :+key-d+ :+key-e+ :+key-f+ :+key-g+ :+key-h+ :+key-i+ :+key-j+
   :+key-k+ :+key-l+ :+key-m+ :+key-n+ :+key-o+ :+key-p+ :+key-q+ :+key-r+ :+key-s+ :+key-t+
   :+key-u+ :+key-v+ :+key-w+ :+key-x+ :+key-y+ :+key-z+ :+key-f1+ :+key-f2+ :+key-f3+ :+key-f4+
   :+key-f5+ :+key-f6+ :+key-f7+ :+key-f8+ :+key-f9+ :+key-f10+ :+key-f11+ :+key-f12+ :+key-one+
   :+key-two+ :+key-three+ :+key-four+ :+key-five+ :+key-six+ :+key-seven+ :+key-eight+ :+key-nine+
   :+key-zero+ :+key-backslash+ :+key-comma+ :+key-delete+ :+key-end+ :+key-equal+ :+key-grave+
   :+key-home+ :+key-insert+ :+key-kp-0+ :+key-kp-1+ :+key-kp-2+ :+key-kp-3+ :+key-kp-4+ :+key-kp-5+
   :+key-kp-6+ :+key-kp-7+ :+key-kp-8+ :+key-kp-9+ :+key-kp-add+ :+key-kp-decimal+ :+key-kp-divide+
   :+key-kp-enter+ :+key-kp-equal+ :+key-kp-multiply+ :+key-kp-subtract+ :+key-left+ :+key-right+
   :+key-up+ :+key-down+ :+key-left-bracket+ :+key-right-bracket+ :+key-left-shift+ :+key-right-shift+
   :+key-left-alt+ :+key-right-alt+ :+key-left-control+ :+key-right-control+ :+key-left-super+
   :+key-right-super+ :+key-minus+ :+key-num-lock+ :+key-page-down+ :+key-page-up+ :+key-pause+
   :+key-print-screen+ :+key-scroll-lock+ :+key-slash+ :+key-volume-up+ :+key-volume-down+
   :+key-apostrophe+ :+key-back+ :+key-backspace+ :+key-caps-lock+ :+key-enter+ :+key-escape+
   :+key-kb-menu+ :+key-menu+ :+key-null+ :+key-period+ :+key-semicolon+ :+key-space+ :+key-tab+

   ;; Gestures
   :+gesture-tap+ :+gesture-drag+ :+gesture-hold+ :+gesture-none+ :+gesture-doubletap+
   :+gesture-pinch-in+ :+gesture-pinch-out+ :+gesture-swipe-down+ :+gesture-swipe-left+
   :+gesture-swipe-right+ :+gesture-swipe-up+

   ;; Mouse
   :+mouse-button-left+ :+mouse-button-right+ :+mouse-button-middle+ :+mouse-button-side+
   :+mouse-button-extra+ :+mouse-button-forward+ :+mouse-button-back+

   ;; Mouse cursor
   :+mouse-cursor-default+ :+mouse-cursor-arrow+ :+mouse-cursor-ibeam+ :+mouse-cursor-crosshair+
   :+mouse-cursor-pointing-hand+ :+mouse-cursor-resize-ew+ :+mouse-cursor-resize-ns+
   :+mouse-cursor-resize-nwse+ :+mouse-cursor-resize-nesw+ :+mouse-cursor-resize-all+
   :+mouse-cursor-not-allowed+
   
   ;; Gamepad
   :+gamepad-axis-left-x+ :+gamepad-axis-left-y+ :+gamepad-axis-right-x+ :+gamepad-axis-right-y+
   :+gamepax-axis-left-trigger+ :+gamepad-axis-right-trigger+ :+gamepad-button-left-face-up+
   :+gamepad-button-left-face-right+ :+gamepad-button-left-face-down+ :+gamepad-button-left-face-left+
   :+gamepad-button-right-face-up+ :+gamepad-button-right-face-right :+gamepad-button-right-face-down+
   :+gamepad-button-right-face-left+ :+gamepad-button-left-trigger-1+ :+gamepad-button-left-trigger-2+
   :+gamepad-button-right-trigger-1+ :+gamepad-button-right-trigger-2+ :+gamepad-button-middle-left+
   :+gamepad-button-middle+ :+gamepad-button-middle-right+ :+gamepad-button-left-thumb+
   :+gamepad-button-right-thumb+ :+gamepad-button-unknown+

   ;; Shader
   :+shader-loc-vertex-position+ :+shader-loc-vertex-texcoord01+ :+shader-loc-vertex-texcoord02+
   :+shader-loc-vertex-normal+ :+shader-loc-vertex-tangent+ :+shader-loc-vertex-color+
   :+shader-loc-matrix-mvp+ :+shader-loc-matrix-view+ :+shader-loc-matrix-projection+
   :+shader-loc-matrix-model+ :+shader-loc-matrix-normal+ :+shader-loc-vector-view+
   :+shader-loc-color-diffuse+ :+shader-loc-color-specular+ :+shader-loc-color-ambient+
   :+shader-loc-map-albedo+ :+shader-loc-map-metalness+ :+shader-loc-map-normal+
   :+shader-loc-map-roughness+ :+shader-loc-map-occlusion+ :+shader-loc-map-emission+
   :+shader-loc-map-height+ :+shader-loc-map-cubemap+ :+shader-loc-map-irradiance+
   :+shader-loc-map-prefilter+ :+shader-loc-map-brdf+
   
   ;; Shader attributes
   :+shader-attrib-float+ :+shader-attrib-vec2+ :+shader-attrib-vec3+ :+shader-attrib-vec4+

   ;; Shader uniforms
   :+shader-uniform-float+ :+shader-uniform-vec2+ :+shader-uniform-vec3+ :+shader-uniform-vec4+
   :+shader-uniform-int+ :+shader-uniform-ivec2+ :+shader-uniform-ivec3+ :+shader-uniform-ivec4+
   :+shader-uniform-sampler2d+
   
   ;; Material maps
   :+material-map-albedo+ :+material-map-brdf+ :+material-map-cubemap+ :+material-map-emission+
   :+material-map-height+ :+material-map-irradiance+ :+material-map-metalness+ :+material-map-normal+
   :+material-map-occlusion+ :+material-map-prefilter+ :+material-map-roughness+

   ;; Cubemap layouts
   :+cubemap-layout-auto-detect+ :+cubemap-layout-line-vertical+ :+cubemap-layout-line-horizontal+
   :+cubemap-layout-cross-three-by-four+ :+cubemap-layout-cross-four-by-three+
   :+cubemap-layout-panorama+

   ;; Texture filters
   :+texture-filter-point+ :+texture-filter-bilinear+ :+texture-filter-trilinear+
   :+texture-filter-anisotropic-4x+ :+texture-filter-anisotropic-8x+ :+texture-filter-anisotropic-16x+

   ;; Texture wrapping
   :+texture-wrap-repeat+ :+texture-wrap-clamp+ :+texture-wrap-mirror-repeat+ :+texture-wrap-mirror-clamp+
   
   ;; Pixel formats
   :+pixelformat-uncompressed-grayscale+ :+pixelformat-uncompressed-gray-alpha+
   :+pixelformat-uncompressed-r5g6b5+ :+pixelformat-uncompressed-r8g8b8+
   :+pixelformat-uncompressed-r5g5b5a1+ :+pixelformat-uncompressed-r4g4b4a4+
   :+pixelformat-uncompressed-r8g8b8a8+ :+pixelformat-uncompressed-r32+
   :+pixelformat-uncompressed-r32g32b32+ :+pixelformat-uncompressed-r32g32b32a32+
   :+pixelformat-compressed-dxt1-rgb+ :+pixelformat-compressed-dxt1-rgba+
   :+pixelformat-compressed-dxt3-rgba+ :+pixelformat-compressed-dxt5-rgba+
   :+pixelformat-compressed-etc1-rgb+ :+pixelformat-compressed-etc2-rgb+
   :+pixelformat-compressed-etc2-eac-rgba+ :+pixelformat-compressed-pvrt-rgb+
   :+pixelformat-compressed-pvrt-rgba+ :+pixelformat-compressed-astc-4x4-rgba+
   :+pixelformat-compressed-astc-8x8-rgba+
   
   ;; Npatch
   :+npatch-nine-patch+ :+npatch-three-patch-vertical+ :+npatch-three-patch-horizontal+
   
   ;; Font types
   :+font-default+ :+font-bitmap+ :+font-sdf+
   
   ;; Camera
   :+camera-perspective+ :+camera-orthographic+ :+camera-free+ :+camera-orbital+ :+camera-first-person+
   :+camera-third-person+ :+camera-custom+

   ;; Window flags
   :+flag-fullscreen-mode+ :+flag-window-resizable+ :+flag-window-undecorated+ :+flag-window-transparent+
   :+flag-msaa-4x-hint+ :+flag-vsync-hint+ :+flag-window-hidden+ :+flag-window-always-run+
   :+flag-window-minimized+ :+flag-window-maximized+ :+flag-window-unfocused+ :+flag-window-topmost+
   :+flag-window-highdpi+ :+flag-interlaced-hint+

   ;; Logging
   :+log-all+ :+log-trace+ :+log-debug+ :+log-info+ :+log-warning+ :+log-error+ :+log-fatal+ :+log-none+

   ;;; Raymath

   ;; Vector2
   :vector2-add :vector2-add-value :vector2-angle :vector2-distance :vector2-divide :vector2-dot-product
   :vector2-length :vector2-length-sqr :vector2-lerp :vector2-move-towards :vector2-multiply
   :vector2-negate :vector2-normalize :vector2-one :vector2-reflect :vector2-rotate :vector2-scale
   :vector2-subtract :vector2-subtract-value :vector2-zero

   ;; Vector3
   :vector3-add :vector3-add-value :vector3-angle :vector3-barycenter :vector3-cross-product
   :vector3-distance :vector3-divide :vector3-dot-product :vector3-length :vector3-length-sqr
   :vector3-lerp :vector3-max :vector3-min :vector3-multiply :vector3-negate :vector3-normalize
   :vector3-one :vector3-ortho-normalize :vector3-perpendicular :vector3-reflect
   :vector3-rotate-by-quaternion :vector3-scale :vector3-subtract :vector3-subtract-value
   :vector3-to-float-v :vector3-transform :vector3-unproject :vector3-zero

   ;; Quaternion
   :quaternion-from-euler :quaternion-from-matrix :quaternion-to-euler :quaternion-to-matrix
   :quaternion-add :quaternion-add-value :quaternion-divide :quaternion-from-axis-angle
   :quaternion-from-vector3-to-vector3 :quaternion-identity :quaternion-invert :quaternion-length
   :quaternion-lerp :quaternion-multiply :quaternion-nlerp :quaternion-normalize :quaternion-scale
   :quaternion-slerp :quaternion-subtract :quaternion-subtract-value :quaternion-to-axis-angle
   :quaternion-transform

   ;; Matrix
   :matrix-rotate-zyx :matrix-add :matrix-determinant :matrix-frustum :matrix-identity :matrix-invert
   :matrix-look-at :matrix-multiply :matrix-normalize :matrix-ortho :matrix-perspective :matrix-rotate
   :matrix-rotate-x :matrix-rotate-xyz :matrix-rotate-y :matrix-rotate-z :matrix-scale :matrix-subtract
   :matrix-to-float-v :matrix-trace :matrix-translate :matrix-transpose
   
   ;;; claylib/ll extras
   
   ;; Convenience wrappers
   :close-window-p :run-window-p :with-window :loop-drawing :do-drawing :with-drawing :with-mode2d
   :with-mode3d :set-vector2 :set-vector3 :set-vector4 :with-texture-mode :with-scissor-mode

   ;; Globals
   :*claylib-background* :*screen-width* :*screen-height* :*target-fps*))
