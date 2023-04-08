;;;; package.lisp
(defpackage #:claylib
  (:use #:static-dispatch-cl #:claylib/ll)
  (:local-nicknames (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:shadow

   ;;; Shadowed symbols
   ;;; Most of these are shadowed because we "override" them with lispier versions.

   ;; Structs
   :model :texture :shader :model-animation :font :rectangle :color :ray :ray-collision :matrix
   :vector2 :vector3 :vector4 :bounding-box :image :glyph-info :material-map :material :mesh
   :render-texture :transform :music :sound

   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+
   :+green+ :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+
   :+beige+ :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+

   ;; Math
   :quaternion-from-euler :quaternion-to-euler :vector2-subtract :vector2-add :vector2-scale :vector2-length
   :matrix-rotate-zyx :quaternion-to-matrix :quaternion-from-matrix :wrap :vector3-subtract
   :vector3-rotate-by-axis-angle :vector3-add :vector3-angle :vector3-negate :vector3-normalize
   :vector3-distance :matrix-multiply :quaternion-multiply :vector3-transform

   ;; Globals
   :*claylib-background*

   ;; Raygui
   :gui-set-font :gui-get-font :gui-window-box :gui-group-box :gui-line :gui-panel :gui-scroll-panel
   :gui-label :gui-button :gui-label-button :gui-toggle :gui-toggle-group :gui-checkbox :gui-combo-box
   :gui-dropdown-box :gui-spinner :gui-value-box :gui-text-box :gui-text-box-multi :gui-slider
   :gui-slider-bar :gui-progress-bar :gui-status-bar :gui-dummy-rec :gui-scroll-bar :gui-grid
   :gui-list-view :gui-list-view-ex :gui-message-box :gui-text-input-box :gui-color-picker :gui-color-panel
   :gui-color-bar-alpha :gui-color-bar-hue :gui-draw-icon :gui-check-icon-pixel :gui-load-style

   ;; rcamera
   :get-camera-forward :get-camera-up :get-camera-right :camera-move-forward :camera-move-up
   :camera-move-right :camera-move-to-target :camera-yaw :camera-pitch :camera-roll
   :get-camera-view-matrix :get-camera-projection-matrix

   ;; Music management
   :is-music-stream-playing-p :seek-music-stream :get-music-time-length :get-music-time-played

   ;; Misc.
   :get-mouse-position :get-mouse-ray :get-ray-collision-box :load-render-texture :with-texture-mode
   :draw-grid :draw-text :draw-rectangle :draw-circle :draw-cube :update-camera :gen-mesh-cylinder
   :with-window :load-font :load-model-from-mesh :gen-mesh-cube :gen-image-checked :load-texture-from-image
   :fade :check-collision-point-rec :clear-background :export-image :load-image-from-texture
   :image-flip-vertical :image-flip-horizontal :image-crop :image-resize :image-draw :with-drawing
   :check-collision-recs :get-collision-rec :check-collision-point-circle :measure-text-ex
   :update-model-animation :set-material-texture :check-collision-boxes :check-collision-box-sphere
   :gen-mesh-cubicmap :get-color :check-collision-circles :check-collision-circle-rec
   :check-collision-point-triangle :check-collision-lines :check-collision-point-line :get-glyph-index
   :get-mouse-delta :get-mouse-wheel-move-v :get-model-bounding-box :update-camera-pro
   :get-world-to-screen-2d :get-screen-to-world-2d :get-world-to-screen-3d :gen-mesh-poly :gen-mesh-plane
   :gen-mesh-sphere :gen-mesh-hemi-sphere :gen-mesh-cone :gen-mesh-torus :gen-mesh-knot :gen-mesh-heightmap)



  (:export

   ;;; Core

   ;; Window-related functions
   :init-window :window-should-close-p :close-window :is-window-ready-p :is-window-fullscreen-p
   :is-window-hidden-p :is-window-minimized-p :is-window-maximized-p :is-window-focused-p
   :is-window-resized-p :is-window-state-p :set-window-state :clear-window-state :toggle-fullscreen
   :maximize-window :minimize-window :restore-window :set-window-title
   :set-window-position :set-window-monitor :set-window-min-size :set-window-size :set-window-opacity
   :get-screen-width :get-screen-height :get-render-width :get-render-height
   :get-monitor-count :get-current-monitor :get-monitor-width :get-monitor-height
   :get-monitor-physical-width :get-monitor-physical-height :get-monitor-refresh-rate
   :get-monitor-name :set-clipboard-text :get-clipboard-text
   :enable-event-waiting :disable-event-waiting

   ;; Custom frame control functions
   ;; Raylib must be specifically compiled to support these (the one we distribute is not)
   :swap-screen-buffer :wait-time

   ;; Cursor-related functions
   :show-cursor :hide-cursor :is-cursor-hidden-p :enable-cursor :disable-cursor :is-cursor-on-screen-p

   ;; Drawing-related functions
   :clear-background :begin-drawing :end-drawing
   :begin-blend-mode
   :end-blend-mode :begin-scissor-mode :end-scissor-mode

   ;; VR stereo config functions for VR simulator

   ;; Shader management functions

   ;; Screen-space-related functions
   :get-mouse-ray :get-world-to-screen-3d :get-screen-to-world-2d :get-world-to-screen-2d

   ;; Timing-related functions
   :set-target-fps :get-fps :get-frame-time :get-time

   ;; Misc. functions
   :get-random-value :set-random-seed :take-screenshot :set-config-flags :trace-log :set-trace-log-level
   :open-url

   ;; Set custom callbacks

   ;; Files management functions
   :save-file-data :export-data-as-code :load-file-text
   :unload-file-text :save-file-text :file-exists-p :directory-exists-p :is-file-extension-p
   :get-file-length :get-file-extension :get-file-name :get-file-name-without-ext :get-directory-path
   :get-prev-directory-path :get-working-directory :get-application-directory :change-directory
   :is-path-file-p
   :is-file-dropped-p :get-file-mod-time

   ;; Compression/Encoding functionality

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
   :get-touch-x :get-touch-y :get-touch-point-id :get-touch-point-count

   ;; Gestures and Touch Handling Functions (Module: rgestures)
   :set-gestures-enabled :is-gesture-detected-p :get-gesture-detected :get-gesture-hold-duration
   :get-gesture-drag-angle :get-gesture-pinch-angle

   ;; Camera System Functions (Module: rcamera)
   :update-camera :set-camera-pan-control :set-camera-alt-control
   :set-camera-smooth-zoom-control :set-camera-move-controls



   ;;; Shapes

   ;; Set texture and rectangle to be used on shapes drawing

   ;; Basic shapes drawing functions

   ;; Basic shapes collision detection functions
   :check-collision-recs :check-collision-circles :check-collision-circle-rec :check-collision-point-rec
   :check-collision-point-circle :check-collision-point-triangle :check-collision-lines
   :check-collision-point-line :get-collision-rec



   ;;; Textures

   ;; Image loading functions
   :load-image-from-texture :export-image

   ;; Image generation functions
   :gen-image-checked

   ;; Image manipulation functions
   :image-crop :image-resize :image-flip-vertical :image-flip-horizontal

   ;; Image drawing functions
   :image-draw

   ;; Texture loading functions
   :load-texture-from-image :load-render-texture

   ;; Texture configuration functions

   ;; Texture drawing functions

   ;; Color/pixel related functions
   :fade
   :get-color :get-pixel-data-size



   ;;; Text

   ;; Font loading/unloading functions

   ;; Text-drawing functions
   :draw-fps

   ;; Text font info functions
   :measure-text :measure-text-ex :get-glyph-index

   ;; Text codepoints management functions
   :load-codepoints :unload-codepoints :get-codepoint-count :get-codepoint :codepoint-to-utf8
   :text-codepoints-to-utf8

   ;; Text string management functions
   :text-copy :text-is-equal-p :text-length :text-format :text-subtext
   :text-find-index :text-to-upper :text-to-lower :text-to-pascal :text-to-integer



   ;;; Models

   ;; Basic genmetric 3D shapes drawing functions
   :draw-cube
   :draw-grid

   ;; Model loading/unloading functions
   :load-model-from-mesh :get-model-bounding-box

   ;; Model drawing functions

   ;; Mesh management functions

   ;; Mesh generation functions
   :gen-mesh-poly :gen-mesh-plane :gen-mesh-cube :gen-mesh-sphere :gen-mesh-hemisphere
   :gen-mesh-cylinder :gen-mesh-cone :gen-mesh-torus :gen-mesh-knot :gen-mesh-heightmap
   :gen-mesh-cubicmap

   ;; Material loading/unloading functions
   :set-material-texture

   ;; Model animations loading/unloading functions
   :update-model-animation

   ;; Collision detection functions
   :check-collision-boxes :check-collision-box-sphere :get-ray-collision-box



   ;;; Audio

   ;; Audio device management functions
   :init-audio-device :close-audio-device :is-audio-device-ready-p :set-master-volume

   ;; Wave/Sound loading/unloading functions

   ;; Wave/Sound management functions
   :stop-sound-multi :get-sounds-playing

   ;; Music management functions
   :is-music-stream-playing-p
   :seek-music-stream
   :get-music-time-length :get-music-time-played

   ;; AudioStream management functions
   :set-audio-stream-buffer-size-default



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
   :+camera-first-person+ :+camera-third-person+ :+camera-pro+

   ;; Window flags
   :+flag-fullscreen-mode+ :+flag-window-resizable+ :+flag-window-undecorated+ :+flag-window-transparent+
   :+flag-msaa-4x-hint+ :+flag-vsync-hint+ :+flag-window-hidden+ :+flag-window-always-run+
   :+flag-window-minimized+ :+flag-window-maximized+ :+flag-window-unfocused+ :+flag-window-topmost+
   :+flag-window-highdpi+ :+flag-window-mouse-passthrough+ :+flag-interlaced-hint+

   ;; Logging
   :+log-all+ :+log-trace+ :+log-debug+ :+log-info+ :+log-warning+ :+log-error+ :+log-fatal+ :+log-none+



   ;;; Math (pass-throughs to Raymath)

   ;; Utils
   :normalize :remap :float-equals

   ;; Vector2
   :vector2-add :vector2-length :vector2-scale :vector2-subtract

   ;; Vector3
   :vector3-add :vector3-subtract :vector3-angle :vector3-negate :vector3-normalize :vector3-transform
   :vector3-rotate-by-axis-angle :vector3-distance

   ;; Quaternion
   :quaternion-multiply :quaternion-from-euler :quaternion-from-matrix :quaternion-to-euler
   :quaternion-to-matrix

   ;; Matrix
   :matrix-multiply :matrix-rotate-zyx



   ;;; Camera (pass-throughs to rcamera)
   :get-camera-forward :get-camera-up :get-camera-right :camera-move-forward :camera-move-up
   :camera-move-right :camera-move-to-target :camera-yaw :camera-pitch :camera-roll
   :get-camera-view-matrix :get-camera-projection-matrix



   ;;; GUI

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
   :gui-list-view :gui-message-box :gui-text-input-box :gui-color-picker :gui-color-panel
   :gui-color-bar-alpha :gui-color-bar-hue

   ;; Styles loading functions
   :gui-load-style :gui-load-style-default

   ;; Icons functionality
   :gui-icon-text :gui-draw-icon :gui-icon :gui-get-icons :gui-get-icon-data :gui-set-icon-data
   :gui-set-icon-pixel :gui-clear-icon-pixel :gui-check-icon-pixel



   ;;; GUI constants

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



   ;;; Claylib extensions

   ;; rl-* classes (should be internal-only?)
   :rl-bounding-box :rl-camera-2d :rl-camera-3d :rl-color :rl-material :rl-material-map :rl-matrix
   :rl-mesh :rl-model :rl-ray :rl-ray-collision :rl-rectangle :rl-shader :rl-texture :rl-transform
   :rl-vector2 :rl-vector3 :rl-vector4

   ;; Claylib classes
   :2d-object :2d-shape :3d-object :3d-shape :animation-asset :camera-3d :circle :cube :font-asset
   :game-scene :glyph-info :grid :image :image-asset :line-2d :material :material-map :mesh :model
   :model-asset :music :pixel :plane :ray :rectangle :render-texture :shader-asset :sound :sphere :text
   :texture :texture-asset :transform :triangle :triangle-3d

   ;; Constructors
   :make-animation-asset :make-billboard :make-camera-2d :make-camera-2d-from-vecs :make-camera-3d
   :make-camera-3d-from-vecs :make-circle :make-color :make-cube :make-cube-from-vecs
   :make-empty-texture :make-font :make-font-asset :make-grid :make-gui-button :make-gui-checkbox
   :make-gui-color-bar-alpha :make-gui-color-bar-hue :make-gui-color-panel :make-gui-color-picker
   :make-gui-combo-box :make-gui-dropdown-box :make-gui-dummy-rec :make-gui-grid :make-gui-group-box
   :make-gui-icon :make-gui-label :make-gui-label-button :make-gui-line :make-gui-list-view
   :make-gui-message-box :make-gui-panel :make-gui-progress-bar :make-gui-scroll-panel
   :make-gui-scroll-panel-from-recs :make-gui-slider :make-gui-slider-bar :make-gui-spinner
   :make-gui-status-bar :make-gui-text-box :make-gui-text-box-multi :make-gui-text-input-box
   :make-gui-toggle :make-gui-toggle-group :make-gui-value-box :make-gui-window-box :make-image
   :make-image-asset :make-line-2d :make-model :make-model-asset :make-music-asset :make-pixel
   :make-plane :make-polygon :make-ray :make-ray-collision :make-rectangle :make-rectangle-from-vecs
   :make-shader-asset :make-simple-image :make-simple-rec :make-sphere :make-sphere-from-vec
   :make-sound-asset :make-text :make-texture :make-texture-asset :make-texture-from-rec :make-triangle
   :make-triangle-3d :make-triangle-3d-from-vecs :make-triangle-from-vecs :make-vector2 :make-vector3
   :make-vector4 :make-zero-matrix

   ;; Scenes/assets
   :add-to-scene :assets :draw-objects :draw-scene :draw-scene-all :draw-scene-except :draw-scene-regex
   :load-scene-all :make-scene :make-scene-pro :objects :params :remove-from-scene :scene-asset
   :scene-object :scene-param :set-up-scene :tear-down-scene :unbind-scene :with-scenes

   ;; Static dispatch draw functions
   :draw-billboard-object :draw-circle-object :draw-cube-object :draw-grid-object :draw-gui-button-object
   :draw-gui-checkbox-object :draw-gui-color-bar-alpha-object :draw-gui-color-bar-hue-object
   :draw-gui-color-panel-object :draw-gui-color-picker-object :draw-gui-combo-box-object
   :draw-gui-dropdown-box-object :draw-gui-dummy-rec-object :draw-gui-grid-object :draw-gui-group-box-object
   :draw-gui-icon-object :draw-gui-label-button-object :draw-gui-label-object :draw-gui-line-object
   :draw-gui-list-view-object :draw-gui-progress-bar-object :draw-gui-scroll-panel-object
   :draw-gui-slider-bar-object :draw-gui-slider-object :draw-gui-spinner-object :draw-gui-status-bar-object
   :draw-gui-text-box-object :draw-text-box-multi-object :draw-gui-text-input-box-object
   :draw-gui-toggle-group-object :draw-gui-toggle-object :draw-gui-value-box-object
   :draw-gui-window-box-object :draw-line-object :draw-model-object :draw-pixel-object :draw-plane-object
   :draw-polygon-object :draw-ray-object :draw-rectangle-object :draw-sphere-object :draw-text-object
   :draw-texture-non-object :draw-texture-object :draw-triangle-3d-object :draw-triangle-object

   ;; Linking
   :link-objects :unlink-objects

   ;; Allocation pools
   :alloc-pool :get-alloc :make-alloc-pool :rem-alloc :set-alloc

   ;; Copy functions
   :copy-color

   ;; Generic functions/methods
   :x :y :z
   :x1 :x2 :x3 :y1 :y2 :y3
   :v1 :v2 :v3
   :r :g :b :a
   :x-scale :y-scale
   :color :filled :height :len :pos :radius :rot :size :width :bbox :rot-axis :rot-angle
   :target :offset :up :zoom :mode :movement :fovy :projection
   :low :high
   :start :end
   :animations :bones :frame-count :frame-poses :maps :materials :trans
   :dest :filter :origin :source :tint
   :font :spacing
   :hit :distance
   :bezier
   :asset :load-asset
   :draw-object
   :set-slot
   :looping :pan :pause :pitch :play :resume :stop :update :volume
   :active :alpha :bounds :buttons :checked :content :edit-mode :focus :icon-id :max-value :message
   :min-value :pixel-size :pressed :scroll :scroll-index :secret-view-active :selected :subdivs
   :text-left :text-right :text-size :title :value :view
   :children

   ;; Globals
   :*claylib-background* :*screen-width* :*screen-height* :*target-fps* :+claylib-directory+

   ;; Convenience macros
   :do-game-loop :with-2d-mode :with-3d-mode :with-audio-device :with-drawing :with-scene-assets
   :with-scene-bindings :with-scene-objects :with-scene-params :with-scissor-mode :with-texture-mode
   :with-window))
