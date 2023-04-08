(defpackage #:claylib/ll
  (:use #:cl #:claylib/wrap)
  (:shadowing-import-from :cl
   ;; These are field names which are needed in claylib/wrap yet conflict with CL symbols.
   :stream :count :format :max :position :min)
  (:export

   ;;; Core

   ;; Window-related functions
   :init-window :window-should-close-p :close-window :is-window-ready-p :is-window-fullscreen-p
   :is-window-hidden-p :is-window-minimized-p :is-window-maximized-p :is-window-focused-p
   :is-window-resized-p :is-window-state-p :set-window-state :clear-window-state :toggle-fullscreen
   :maximize-window :minimize-window :restore-window :set-window-icon :set-window-title
   :set-window-position :set-window-monitor :set-window-min-size :set-window-size :set-window-opacity
   :get-window-handle :get-screen-width :get-screen-height :get-render-width :get-render-height
   :get-monitor-count :get-current-monitor :get-monitor-position :get-monitor-width :get-monitor-height
   :get-monitor-physical-width :get-monitor-physical-height :get-monitor-refresh-rate
   :get-window-position :get-window-scale-dpi :get-monitor-name :set-clipboard-text :get-clipboard-text
   :enable-event-waiting :disable-event-waiting

   ;; Custom frame control functions
   ;; Raylib must be specifically compiled to support these (the one we distribute is not)
   :swap-screen-buffer :poll-input-events :wait-time

   ;; Cursor-related functions
   :show-cursor :hide-cursor :is-cursor-hidden-p :enable-cursor :disable-cursor :is-cursor-on-screen-p

   ;; Drawing-related functions
   :clear-background :begin-drawing :end-drawing :begin-mode-2d :end-mode-2d :begin-mode-3d :end-mode-3d
   :begin-texture-mode :end-texture-mode :begin-shader-mode :end-shader-mode :begin-blend-mode
   :end-blend-mode :begin-scissor-mode :end-scissor-mode :begin-vr-stereo-mode :end-vr-stereo-mode

   ;; VR stereo config functions for VR simulator
   :load-vr-stereo-config :unload-vr-stereo-config

   ;; Shader management functions
   :load-shader :load-shader-from-memory :is-shader-ready-p :get-shader-location
   :get-shader-location-attrib :set-shader-value :set-shader-value-v :set-shader-value-matrix
   :set-shader-value-texture :unload-shader

   ;; Screen-space-related functions
   :get-mouse-ray :get-camera-matrix :get-camera-matrix-2d :get-world-to-screen :get-screen-to-world-2d
   :get-world-to-screen-ex :get-world-to-screen-2d

   ;; Timing-related functions
   :set-target-fps :get-fps :get-frame-time :get-time

   ;; Misc. functions
   :get-random-value :set-random-seed :take-screenshot :set-config-flags :trace-log :set-trace-log-level
   :mem-alloc :mem-realloc :mem-free :open-url

   ;; Set custom callbacks
   :set-trace-log-callback :set-load-file-data-callback :set-save-file-data-callback
   :set-load-file-text-callback :set-save-file-text-callback

   ;; Files management functions
   :load-file-data :unload-file-data :save-file-data :export-data-as-code :load-file-text
   :unload-file-text :save-file-text :file-exists-p :directory-exists-p :is-file-extension-p
   :get-file-length :get-file-extension :get-file-name :get-file-name-without-ext :get-directory-path
   :get-prev-directory-path :get-working-directory :get-application-directory :change-directory
   :is-path-file-p :load-directory-files :load-directory-files-ex :unload-directory-files
   :is-file-dropped-p :load-dropped-files :unload-dropped-files :get-file-mod-time

   ;; Compression/Encoding functionality
   :compress-data :decompress-data :encode-data-base64 :decode-data-base64

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
   :set-mouse-scale :get-mouse-wheel-move :get-mouse-wheel-move-v :set-mouse-cursor

   ;; Input-related functions: touch
   :get-touch-x :get-touch-y :get-touch-position :get-touch-point-id :get-touch-point-count

   ;; Gestures and Touch Handling Functions (Module: rgestures)
   :set-gestures-enabled :is-gesture-detected-p :get-gesture-detected :get-gesture-hold-duration
   :get-gesture-drag-vector :get-gesture-drag-angle :get-gesture-pinch-vector :get-gesture-pinch-angle

   ;; Camera System Functions (Module: rcamera)
   :update-camera :update-camera-pro



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
   :load-image-from-screen :is-image-ready-p :unload-image :export-image :export-image-as-code

   ;; Image generation functions
   :gen-image-color :gen-image-gradient-v :gen-image-gradient-h :gen-image-gradient-radial
   :gen-image-checked :gen-image-white-noise :gen-image-cellular

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
   :load-texture :load-texture-from-image :load-texture-cubemap :load-render-texture :is-texture-ready-p
   :unload-texture :is-render-texture-ready-p :unload-render-texture :update-texture :update-texture-rec

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
   :is-font-ready-p :load-font-data :gen-image-font-atlas :unload-font-data :unload-font
   :export-font-as-code

   ;; Text-drawing functions
   :draw-fps :draw-text :draw-text-ex :draw-text-pro :draw-text-codepoint :draw-text-codepoints

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
   :load-model :load-model-from-mesh :is-model-ready-p :unload-model :unload-model-keep-meshes
   :get-model-bounding-box

   ;; Model drawing functions
   :draw-model :draw-model-ex :draw-model-wires :draw-model-wires-ex :draw-bounding-box
   :draw-billboard :draw-billboard-rec :draw-billboard-pro

   ;; Mesh management functions
   :upload-mesh :update-mesh-buffer :unload-mesh :draw-mesh :draw-mesh-instanced :export-mesh
   :get-mesh-bounding-box :gen-mesh-tangents

   ;; Mesh generation functions
   :gen-mesh-poly :gen-mesh-plane :gen-mesh-cube :gen-mesh-sphere :gen-mesh-hemi-sphere
   :gen-mesh-cylinder :gen-mesh-cone :gen-mesh-torus :gen-mesh-knot :gen-mesh-heightmap
   :gen-mesh-cubicmap

   ;; Material loading/unloading functions
   :load-materials :load-material-default :is-material-ready-p :unload-material :set-material-texture
   :set-model-mesh-material

   ;; Model animations loading/unloading functions
   :load-model-animations :update-model-animation :unload-model-animation :unload-model-animations
   :is-model-animation-valid-p

   ;; Collision detection functions
   :check-collision-spheres :check-collision-boxes :check-collision-box-sphere :get-ray-collision-sphere
   :get-ray-collision-box :get-ray-collision-mesh :get-ray-collision-triangle :get-ray-collision-quad



   ;;; Audio

   ;; Audio device management functions
   :init-audio-device :close-audio-device :is-audio-device-ready-p :set-master-volume

   ;; Wave/Sound loading/unloading functions
   :load-wave :load-wave-from-memory :is-wave-ready-p :load-sound :load-sound-from-wave
   :is-sound-ready-p :update-sound :unload-wave :unload-sound :export-wave :export-wave-as-code

   ;; Wave/Sound management functions
   :play-sound :stop-sound :pause-sound :resume-sound :play-sound-multi :stop-sound-multi
   :get-sounds-playing :is-sound-playing-p :set-sound-volume :set-sound-pitch :set-sound-pan
   :wave-copy :wave-crop :wave-format :load-wave-samples :unload-wave-samples

   ;; Music management functions
   :load-music-stream :load-music-stream-from-memory :is-music-ready-p :unload-music-stream
   :play-music-stream :is-music-stream-playing-p :update-music-stream :stop-music-stream
   :pause-music-stream :resume-music-stream :seek-music-stream :set-music-volume :set-music-pitch
   :set-music-pan :get-music-time-length :get-music-time-played

   ;; AudioStream management functions
   :load-audio-stream :is-audio-stream-ready-p :unload-audio-stream :update-audio-stream
   :is-audio-stream-processed-p :play-audio-stream :pause-audio-stream :resume-audio-stream
   :is-audio-stream-playing-p :stop-audio-stream :set-audio-stream-volume :set-audio-stream-pitch
   :set-audio-stream-pan :set-audio-stream-buffer-size-default :set-audio-stream-callback
   :attach-audio-stream-processor :detach-audio-stream-processor



   ;;; Structs

   :vector2 :vector3 :vector4 :quaternion :matrix :color :rectangle :image :texture :texture-2d
   :texture-cubemap :render-texture :render-texture-2d :n-patch-info :glyph-info :font :camera-3d
   :camera-2d :mesh :shader :material-map :material :model :transform :bone-info :model-animation
   :ray :ray-collision :bounding-box :wave :sound :music :audio-stream :vr-device-info :vr-stereo-config



   ;;; Constants

   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+ :+green+
   :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+ :+beige+
   :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+

   ;; Blending
   :+blend-alpha+ :+blend-additive+ :+blend-multiplied+ :+blend-add-colors+ :+blend-subtract-colors+
   :+blend-alpha-premultiply+ :+blend-custom+

   ;; Keys
   :+key-null+ :+key-back+ :+key-volume-up+ :+key-volume-down+ :+key-space+ :+key-apostrophe+
   :+key-comma+ :+key-minus+ :+key-period+ :+key-slash+ :+key-zero+ :+key-one+ :+key-two+ :+key-three+
   :+key-four+ :+key-five+ :+key-six+ :+key-seven+ :+key-eight+ :+key-nine+ :+key-semicolon+
   :+key-equal+ :+key-a+ :+key-b+ :+key-c+ :+key-d+ :+key-e+ :+key-f+ :+key-g+ :+key-h+ :+key-i+
   :+key-j+ :+key-k+ :+key-l+ :+key-m+ :+key-n+ :+key-o+ :+key-p+ :+key-q+ :+key-r+ :+key-s+ :+key-t+
   :+key-u+ :+key-v+ :+key-w+ :+key-x+ :+key-y+ :+key-z+ :+key-left-bracket+ :+key-backslash+
   :+key-right-bracket+ :+key-grave+ :+key-escape+ :+key-enter+ :+key-tab+ :+key-backspace+ :+key-insert+
   :+key-delete+ :+key-right+ :+key-left+ :+key-down+ :+key-up+ :+key-page-up+ :+key-page-down+
   :+key-home+ :+key-end+ :+key-caps-lock+ :+key-scroll-lock+ :+key-num-lock+ :+key-print-screen+
   :+key-pause+ :+key-f1+ :+key-f2+ :+key-f3+ :+key-f4+ :+key-f5+ :+key-f6+ :+key-f7+ :+key-f8+
   :+key-f9+ :+key-f10+ :+key-f11+ :+key-f12+ :+key-kp-0+ :+key-kp-1+ :+key-kp-2+ :+key-kp-3+ :+key-kp-4+
   :+key-kp-5+ :+key-kp-6+ :+key-kp-7+ :+key-kp-8+ :+key-kp-9+ :+key-kp-decimal+ :+key-kp-divide+
   :+key-kp-multiply+ :+key-kp-subtract+ :+key-kp-add+ :+key-kp-enter+ :+key-kp-equal+ :+key-left-shift+
   :+key-left-control+ :+key-left-alt+ :+key-left-super+ :+key-right-shift+ :+key-right-control+
   :+key-right-alt+ :+key-right-super+ :+key-kb-menu+

   ;; Gestures
   :+gesture-none+ :+gesture-tap+ :+gesture-doubletap+ :+gesture-hold+ :+gesture-drag+
   :+gesture-swipe-right+ :+gesture-swipe-left+ :+gesture-swipe-up+ :+gesture-swipe-down+
   :+gesture-pinch-in+ :+gesture-pinch-out+

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
   :+gamepad-axis-left-trigger+ :+gamepad-axis-right-trigger+ :+gamepad-button-unknown+
   :+gamepad-button-left-face-up+ :+gamepad-button-left-face-right+ :+gamepad-button-left-face-down+
   :+gamepad-button-left-face-left+ :+gamepad-button-right-face-up+ :+gamepad-button-right-face-right+
   :+gamepad-button-right-face-down+ :+gamepad-button-right-face-left+ :+gamepad-button-left-trigger-1+
   :+gamepad-button-left-trigger-2+ :+gamepad-button-right-trigger-1+ :+gamepad-button-right-trigger-2+
   :+gamepad-button-middle-left+ :+gamepad-button-middle+ :+gamepad-button-middle-right+
   :+gamepad-button-left-thumb+ :+gamepad-button-right-thumb+

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
   :+material-map-albedo+ :+material-map-diffuse+ :+material-map-metalness+ :+material-map-specular+
   :+material-map-normal+ :+material-map-roughness+ :+material-map-occlusion+ :+material-map-emission+
   :+material-map-height+ :+material-map-cubemap+ :+material-map-irradiance+ :+material-map-prefilter+
   :+material-map-brdf+

   ;; Cubemap layouts
   :+cubemap-layout-auto-detect+ :+cubemap-layout-line-vertical+ :+cubemap-layout-line-horizontal+
   :+cubemap-layout-cross-three-by-four+ :+cubemap-layout-cross-four-by-three+ :+cubemap-layout-panorama+

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
   :+camera-custom+ :+camera-perspective+ :+camera-free+ :+camera-orthographic+ :+camera-orbital+
   :+camera-first-person+ :+camera-third-person+

   ;; Window flags
   :+flag-fullscreen-mode+ :+flag-window-resizable+ :+flag-window-undecorated+ :+flag-window-transparent+
   :+flag-msaa-4x-hint+ :+flag-vsync-hint+ :+flag-window-hidden+ :+flag-window-always-run+
   :+flag-window-minimized+ :+flag-window-maximized+ :+flag-window-unfocused+ :+flag-window-topmost+
   :+flag-window-highdpi+ :+flag-window-mouse-passthrough+ :+flag-interlaced-hint+

   ;; Logging
   :+log-all+ :+log-trace+ :+log-debug+ :+log-info+ :+log-warning+ :+log-error+ :+log-fatal+ :+log-none+



   ;;; Raymath

   ;; Utils
   :normalize :remap :float-equals

   ;; Vector2
   :vector2-add :vector2-add-value :vector2-angle :vector2-clamp :vector2-clamp-value :vector2-distance
   :vector2-distance-sqr :vector2-divide :vector2-dot-product :vector2-equals :vector2-invert
   :vector2-length :vector2-length-sqr :vector2-lerp :vector2-move-towards :vector2-multiply
   :vector2-negate :vector2-normalize :vector2-one :vector2-reflect :vector2-rotate :vector2-scale
   :vector2-subtract :vector2-subtract-value :vector2-transform :vector2-zero

   ;; Vector3
   :vector3-add :vector3-add-value :vector3-angle :vector3-barycenter :vector3-clamp :vector3-clamp-value
   :vector3-cross-product :vector3-distance :vector3-distance-sqr :vector3-divide :vector3-dot-product
   :vector3-equals :vector3-invert :vector3-length :vector3-length-sqr :vector3-lerp :vector3-max
   :vector3-min :vector3-multiply :vector3-negate :vector3-normalize :vector3-one
   :vector3-ortho-normalize :vector3-perpendicular :vector3-reflect :vector3-refract
   :vector3-rotate-by-axis-angle :vector3-rotate-by-quaternion :vector3-scale :vector3-subtract
   :vector3-subtract-value :vector3-to-float-v :vector3-transform :vector3-unproject :vector3-zero

   ;; Quaternion
   :quaternion-add :quaternion-add-value :quaternion-divide :quaternion-equals :quaternion-from-axis-angle
   :quaternion-from-euler :quaternion-from-matrix :quaternion-from-vector3-to-vector3 :quaternion-identity
   :quaternion-invert :quaternion-length :quaternion-lerp :quaternion-multiply :quaternion-nlerp
   :quaternion-normalize :quaternion-scale :quaternion-slerp :quaternion-subtract
   :quaternion-subtract-value :quaternion-to-axis-angle :quaternion-to-euler :quaternion-to-matrix
   :quaternion-transform

   ;; Matrix
   :matrix-add :matrix-determinant :matrix-frustum :matrix-identity :matrix-invert :matrix-look-at
   :matrix-multiply :matrix-ortho :matrix-perspective :matrix-rotate :matrix-rotate-x :matrix-rotate-xyz
   :matrix-rotate-y :matrix-rotate-z :matrix-rotate-zyx :matrix-scale :matrix-subtract :matrix-to-float-v
   :matrix-trace :matrix-translate :matrix-transpose



   ;;; Raygui

   ;; Global gui state control functions
   :gui-enable :gui-disable :gui-lock :gui-unlock :gui-is-locked-p :gui-fade :gui-set-state :gui-get-state

   ;; Font set/get functions
   :gui-set-font :gui-get-font

   ;; Style set/get functions
   :gui-set-style :gui-get-style

   ;; Container/separator controls
   :gui-window-box :gui-group-box :gui-line :gui-panel :gui-scroll-panel

   ;; Basic controls set
   :gui-label :gui-button :gui-label-button :gui-toggle :gui-toggle-group :gui-checkbox :gui-combo-box
   :gui-dropdown-box :gui-spinner :gui-value-box :gui-text-box :gui-text-box-multi :gui-slider
   :gui-slider-bar :gui-progress-bar :gui-status-bar :gui-dummy-rec :gui-grid

   ;; Advance controls set
   :gui-list-view :gui-list-view-ex :gui-message-box :gui-text-input-box :gui-color-picker :gui-color-panel
   :gui-color-bar-alpha :gui-color-bar-hue

   ;; Styles loading functions
   :gui-load-style :gui-load-style-default

   ;; Icons functionality
   :gui-icon-text :gui-draw-icon :gui-get-icons :gui-get-icon-data :gui-set-icon-data :gui-set-icon-scale
   :gui-set-icon-pixel :gui-clear-icon-pixel :gui-check-icon-pixel



   ;;; Raygui constants

   ;; Gui control state
   :+state-normal+ :+state-focused+ :+state-pressed+ :+state-disabled+

   ;; Gui control text alignment
   :+text-align-left+ :+text-align-center+ :+text-align-right+

   ;; Gui controls
   :+default+ :+label+ :+button+ :+toggle+ :+slider+ :+progress-bar+ :+checkbox+ :+combo-box+
   :+dropdown-box+ :+text-box+ :+value-box+ :+spinner+ :+list-view+ :+color-picker+ :+scrollbar+
   :+status-bar+

   ;; Gui base properties for every control
   :+border-color-normal+ :+base-color-normal+ :+text-color-normal+ :+border-color-focused+
   :+base-color-focused+ :+text-color-focused+ :+border-color-pressed+ :+base-color-pressed+
   :+text-color-pressed+ :+border-color-disabled+ :+base-color-disabled+ :+text-color-disabled+
   :+border-width+ :+text-padding+ :+text-alignment+ :+reserved+

   ;; Default extended properties
   :+text-size+ :+text-spacing+ :+line-color+ :+background-color+

   ;; Toggle/ToggleGroup
   :+group-padding+

   ;; Slider/SliderBar
   :+slider-width+ :+slider-padding+

   ;; ProgressBar
   :+progress-padding+

   ;; ScrollBar
   :+arrows-size+ :+arrows-visible+ :+scroll-slider-padding+ :+scroll-slider-size+ :+scroll-padding+
   :+scroll-speed+

   :+scrollbar-left-side+ :+scrollbar-right-side+

   ;; CheckBox
   :+check-padding+

   ;; ComboBox
   :+combo-button-width+ :+combo-button-spacing+

   ;; DropdownBox
   :+arrow-padding+ :+dropdown-items-spacing+

   ;; TextBox/TextBoxMulti/ValueBox/Spinner
   :+text-inner-padding+ :+text-lines-spacing+

   ;; Spinner
   :+spin-button-width+ :+spin-button-scaling+

   ;; ListView
   :+list-items-height+ :+list-items-spacing+ :+scrollbar-width+ :+scrollbar-side+

   ;; ColorPicker
   :+color-selector-size+ :+huebar-width+ :+huebar-padding+ :+huebar-selector-height+
   :+huebar-selector-overflow+

   ;; Icons
   :+icon-none+ :+icon-folder-file-open+ :+icon-file-save-classic+ :+icon-folder-open+ :+icon-folder-save+
   :+icon-file-open+ :+icon-file-save+ :+icon-file-export+ :+icon-file-add+ :+icon-file-delete+
   :+icon-filetype-text+ :+icon-filetype-audio+ :+icon-filetype-image+ :+icon-filetype-play+
   :+icon-filetype-video+ :+icon-filetype-info+ :+icon-file-copy+ :+icon-file-cut+ :+icon-file-paste+
   :+icon-cursor-hand+ :+icon-cursor-pointer+ :+icon-cursor-classic+ :+icon-pencil+ :+icon-pencil-big+
   :+icon-brush-classic+ :+icon-brush-painter+ :+icon-water-drop+ :+icon-color-picker+ :+icon-rubber+
   :+icon-color-bucket+ :+icon-text-t+ :+icon-text-a+ :+icon-scale+ :+icon-resize+ :+icon-filter-point+
   :+icon-filter-bilinear+ :+icon-crop+ :+icon-crop-alpha+ :+icon-square-toggle+ :+icon-symmetry+
   :+icon-symmetry-horizontal+ :+icon-symmetry-vertical+ :+icon-lens+ :+icon-lens-big+ :+icon-eye-on+
   :+icon-eye-off+ :+icon-filter-top+ :+icon-filter+ :+icon-target-point+ :+icon-target-small+
   :+icon-target-big+ :+icon-target-move+ :+icon-cursor-move+ :+icon-cursor-scale+ :+icon-cursor-scale-right+
   :+icon-cursor-scale-left+ :+icon-undo+ :+icon-redo+ :+icon-reredo+ :+icon-mutate+ :+icon-rotate+
   :+icon-repeat+ :+icon-shuffle+ :+icon-emptybox+ :+icon-target+ :+icon-target-small-fill+
   :+icon-target-big-fill+ :+icon-target-move-fill+ :+icon-cursor-move-fill+ :+icon-cursor-scale-fill+
   :+icon-cursor-scale-right-fill+ :+icon-cursor-scale-left-fill+ :+icon-undo-fill+ :+icon-redo-fill+
   :+icon-reredo-fill+ :+icon-mutate-fill+ :+icon-rotate-fill+ :+icon-repeat-fill+ :+icon-shuffle-fill+
   :+icon-emptybox-small+ :+icon-box+ :+icon-box-top+ :+icon-box-top-right+ :+icon-box-right+
   :+icon-box-bottom-right+ :+icon-box-bottom+ :+icon-box-bottom-left+ :+icon-box-left+ :+icon-box-top-left+
   :+icon-box-center+ :+icon-box-circle-mask+ :+icon-pot+ :+icon-alpha-multiply+ :+icon-alpha-clear+
   :+icon-dithering+ :+icon-mipmaps+ :+icon-box-grid+ :+icon-grid+ :+icon-box-corners-small+
   :+icon-box-corners-big+ :+icon-four-boxes+ :+icon-grid-fill+ :+icon-box-multisize+ :+icon-zoom-small+
   :+icon-zoom-medium+ :+icon-zoom-big+ :+icon-zoom-all+ :+icon-zoom-center+ :+icon-box-dots-small+
   :+icon-box-dots-big+ :+icon-box-concentric+ :+icon-box-grid-big+ :+icon-ok-tick+ :+icon-cross+
   :+icon-arrow-left+ :+icon-arrow-right+ :+icon-arrow-down+ :+icon-arrow-up+ :+icon-arrow-left-fill+
   :+icon-arrow-right-fill+ :+icon-arrow-down-fill+ :+icon-arrow-up-fill+ :+icon-audio+ :+icon-fx+
   :+icon-wave+ :+icon-wave-sinus+ :+icon-wave-square+ :+icon-wave-triangular+ :+icon-cross-small+
   :+icon-player-previous+ :+icon-player-play-back+ :+icon-player-play+ :+icon-player-pause+
   :+icon-player-stop+ :+icon-player-next+ :+icon-player-record+ :+icon-magnet+ :+icon-lock-close+
   :+icon-lock-open+ :+icon-clock+ :+icon-tools+ :+icon-gear+ :+icon-gear-big+ :+icon-bin+
   :+icon-hand-pointer+ :+icon-laser+ :+icon-coin+ :+icon-explosion+ :+icon-1up+ :+icon-player+
   :+icon-player-jump+ :+icon-key+ :+icon-demon+ :+icon-text-popup+ :+icon-gear-ex+ :+icon-crack+
   :+icon-crack-points+ :+icon-star+ :+icon-door+ :+icon-exit+ :+icon-mode-2d+ :+icon-mode-3d+ :+icon-cube+
   :+icon-cube-face-top+ :+icon-cube-face-left+ :+icon-cube-face-front+ :+icon-cube-face-bottom+
   :+icon-cube-face-right+ :+icon-cube-face-back+ :+icon-camera+ :+icon-special+ :+icon-link-net+
   :+icon-link-boxes+ :+icon-link-multi+ :+icon-link+ :+icon-link-broke+ :+icon-text-notes+
   :+icon-notebook+ :+icon-suitcase+ :+icon-suitcase-zip+ :+icon-mailbox+ :+icon-monitor+ :+icon-printer+
   :+icon-photo-camera+ :+icon-photo-camera-flash+ :+icon-house+ :+icon-heart+ :+icon-corner+
   :+icon-vertical-bars+ :+icon-vertical-bars-fill+ :+icon-life-bars+ :+icon-info+ :+icon-crossline+
   :+icon-help+ :+icon-filetype-alpha+ :+icon-filetype-home+ :+icon-layers-visible+ :+icon-layers+
   :+icon-window+ :+icon-hidpi+ :+icon-filetype-binary+ :+icon-hex+ :+icon-shield+ :+icon-file-new+
   :+icon-folder-add+ :+icon-alarm+

   ;; Others
   :+raygui-colorbaralpha-checked-size+ :+raygui-grid-alpha+ :+raygui-groupbox-line-thick+
   :+raygui-icon-data-elements+ :+raygui-icon-max-icons+ :+raygui-icon-max-name-length+
   :+raygui-icon-size+ :+raygui-line-margin-text+ :+raygui-line-text-padding+ :+raygui-max-controls+
   :+raygui-max-props-base+ :+raygui-max-props-extended+ :+raygui-messagebox-button-height+
   :+raygui-messagebox-button-padding+ :+raygui-panel-border-width+ :+raygui-textinputbox-button-height+
   :+raygui-textinputbox-button-padding+ :+raygui-textinputbox-height+ :+raygui-textsplit-max-items+
   :+raygui-textsplit-max-text-size+ :+raygui-togglegroup-max-items+ :+raygui-valuebox-max-chars+
   :+raygui-version+ :+raygui-windowbox-statusbar-height+



   ;;; rcamera

   :get-camera-forward :get-camera-up :get-camera-right :camera-move-forward :camera-move-up
   :camera-move-right :camera-move-to-target :camera-yaw :camera-pitch :camera-roll
   :get-camera-view-matrix :get-camera-projection-matrix



   ;;; claylib/ll extras

   ;; Convenience wrappers
   :close-window-p :run-window-p :with-window :loop-drawing :do-drawing :with-drawing :with-mode-2d
   :with-mode-3d :set-vector2 :set-vector3 :set-vector4 :with-texture-mode :with-scissor-mode
   :set-matrix :set-color :set-rectangle :set-image :set-texture :set-render-texture :set-n-patch-info
   :set-glyph-info :set-font :set-camera-3d :set-camera-2d :set-mesh :set-shader :set-material-map
   :set-material :set-transform :set-bone-info :set-model :set-model-animation :set-ray :set-ray-collision
   :set-bounding-box :set-wave :set-audio-stream :set-sound :set-music :data-valid-p :array-valid-p
   :full-copy-image :full-copy-glyph-info :full-copy-font :full-copy-mesh :full-copy-shader
   :full-copy-material :full-copy-model :full-copy-model-animation :full-copy-wave :full-copy-vector2
   :full-copy-vector3 :full-copy-vector4 :full-copy-matrix :full-copy-color :full-copy-rectangle
   :full-copy-texture :full-copy-render-texture :full-copy-n-patch-info :full-copy-camera-3d
   :full-copy-camera-2d :full-copy-material-map :full-copy-transform :full-copy-bone-info :full-copy-ray
   :full-copy-ray-collision :full-copy-bounding-box :full-copy-vr-device-info :full-copy-vr-stereo-config
   :partial-copy-vector2 :partial-copy-vector3 :partial-copy-vector4 :partial-copy-matrix
   :partial-copy-color :partial-copy-rectangle :partial-copy-texture :partial-copy-render-texture
   :partial-copy-n-patch-info :partial-copy-glyph-info :partial-copy-font :partial-copy-camera-3d
   :partial-copy-camera-2d :partial-copy-mesh :partial-copy-shader :partial-copy-material-map
   :partial-copy-material :partial-copy-transform :partial-copy-bone-info :partial-copy-model
   :partial-copy-model-animation :partial-copy-ray :partial-copy-ray-collision :partial-copy-bounding-box
   :partial-copy-vr-device-info :partial-copy-vr-stereo-config :copy-c-array :calloc :field-ptr
   :field-value :safe-unload-audio-stream :safe-unload-font :safe-unload-image :safe-unload-material
   :safe-unload-model :safe-unload-render-texture :safe-unload-shader :safe-unload-sound
   :safe-unload-texture :safe-unload-wave :safe-unload-music

   ;; Globals
   :*claylib-background* :*screen-width* :*screen-height* :*target-fps*))
