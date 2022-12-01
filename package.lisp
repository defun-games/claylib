;;;; package.lisp
(defpackage #:claylib
  (:use #:cl #:plus-c #:claylib/ll)
  (:local-nicknames (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:shadow

   ;;; Shadowed symbols
   ;;; Most of these are shadowed because we "override" them with lispier versions.

   ;; Structs
   :model :texture :shader :model-animation :font :rectangle :color :ray :ray-collision :matrix
   :vector2 :vector3 :vector4 :bounding-box :image :glyph-info :material-map :material :mesh
   :render-texture :transform

   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+
   :+green+ :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+
   :+beige+ :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+

   ;; Math
   :quaternion-from-euler :quaternion-to-euler :vector2-subtract :vector2-add :vector2-scale :vector2-length
   :matrix-rotate-zyx :quaternion-to-matrix :quaternion-from-matrix

   ;; Globals
   :*claylib-background*

   ;; Raygui
   :gui-set-font :gui-get-font :gui-window-box :gui-group-box :gui-line :gui-panel :gui-scroll-panel
   :gui-label :gui-button :gui-label-button :gui-toggle :gui-toggle-group :gui-checkbox :gui-combo-box
   :gui-dropdown-box :gui-spinner :gui-value-box :gui-text-box :gui-text-box-multi :gui-slider
   :gui-slider-bar :gui-progress-bar :gui-status-bar :gui-dummy-rec :gui-scroll-bar :gui-grid
   :gui-list-view :gui-list-view-ex :gui-message-box :gui-text-input-box :gui-color-picker :gui-color-panel
   :gui-color-bar-alpha :gui-color-bar-hue :gui-draw-icon :gui-check-icon-pixel

   ;; Misc.
   :get-mouse-position :get-mouse-ray :get-ray-collision-box :load-render-texture :with-texture-mode
   :draw-grid :draw-text :draw-rectangle :draw-circle :draw-cube :update-camera :gen-mesh-cylinder
   :with-window :load-font :load-model-from-mesh :gen-mesh-cube :gen-image-checked :load-texture-from-image
   :fade :check-collision-point-rec :clear-background :export-image :load-image-from-texture
   :image-flip-vertical :image-flip-horizontal :image-crop :image-resize :image-draw :with-drawing
   :check-collision-recs :get-collision-rec :check-collision-point-circle :measure-text-ex
   :update-model-animation :set-material-texture :check-collision-boxes :check-collision-box-sphere
   :gen-mesh-cubicmap)



  (:export

   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+
   :+green+ :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+
   :+beige+ :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+
   :fade :copy-color

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
   :+mouse-button-extra+ :+mouse-button-forward+ :+mouse-button-back+ :get-mouse-x :get-mouse-y
   :get-mouse-wheel-move :get-mouse-position :set-mouse-offset :set-mouse-scale :set-mouse-cursor

   ;; Mouse cursor
   :+mouse-cursor-default+ :+mouse-cursor-arrow+ :+mouse-cursor-ibeam+ :+mouse-cursor-crosshair+
   :+mouse-cursor-pointing-hand+ :+mouse-cursor-resize-ew+ :+mouse-cursor-resize-ns+
   :+mouse-cursor-resize-nwse+ :+mouse-cursor-resize-nesw+ :+mouse-cursor-resize-all+
   :+mouse-cursor-not-allowed+

   ;; Camera
   :+camera-perspective+ :get-world-to-screen-2d :get-screen-to-world-2d :+camera-third-person+
   :+camera-orthographic+ :+camera-custom+ :+camera-free+ :+camera-orbital+ :+camera-first-person+
   :update-camera :get-world-to-screen-3d

   ;; Material maps
   :+material-map-albedo+ :+material-map-brdf+ :+material-map-cubemap+ :+material-map-emission+
   :+material-map-height+ :+material-map-irradiance+ :+material-map-metalness+ :+material-map-normal+
   :+material-map-occlusion+ :+material-map-prefilter+ :+material-map-roughness+
   :+material-map-diffuse+ :+material-map-specular+

   ;; Types
   :text :rectangle :rl-rectangle :circle :cube :grid :rl-vector2 :rl-vector3 :rl-vector4 :rl-color
   :line-2d :rl-camera-3d :camera-3d :rl-camera-2d :plane :rl-ray :ray :rl-ray-collision :rl-bounding-box
   :rl-texture :texture :rl-transform :rl-model :model :rl-mesh :rl-shader :rl-material-map :rl-material
   :rl-matrix :triangle :image-asset :texture-asset :model-asset :shader-asset :font-asset
   :animation-asset :pixel :image :game-scene :glyph-info :material-map :material :mesh :render-texture
   :transform :sphere

   ;; Misc. convenience wrappers
   :with-window :do-game-loop :with-drawing :is-key-pressed-p :is-gesture-detected-p :is-key-down-p
   :with-scene-objects :is-mouse-button-pressed-p :get-key-pressed :get-char-pressed
   :is-mouse-button-down-p :make-vector2 :make-vector3 :make-vector4 :make-camera-3d
   :make-camera-3d-from-vecs :make-camera-2d :make-camera-2d-from-vecs :make-color :make-rectangle
   :make-rectangle-from-vecs :make-circle :make-grid :make-cube :make-cube-from-vecs :make-text
   :make-line-2d :with-2d-mode :with-3d-mode :make-plane :make-ray :make-ray-collision :with-texture-mode
   :with-scissor-mode :make-zero-matrix :is-window-state-p :make-triangle :make-triangle-from-vecs
   :make-texture :make-texture-from-rec :make-empty-texture :make-texture-asset :get-gesture-detected
   :is-mouse-button-released-p :make-polygon :make-font :make-image-asset :make-pixel :make-image
   :make-simple-rec :make-billboard :make-font-asset :window-should-close-p :make-model-asset
   :make-model :make-animation-asset :make-sphere :make-sphere-from-vec

   ;; Scenes
   :draw-scene-all :scene-object :scene-asset :scene-param :load-scene-all :unload-scene-all
   :draw-scene :draw-scene-except :make-scene :make-scene-pro :params :assets :objects
   :draw-scene-regex :with-scenes :set-up-scene :tear-down-scene :draw-objects

   ;; Generic functions/methods
   :x :y :z :color :target :rot :zoom :x1 :y1 :x2 :y2 :width :height :len :offset :pos :draw-object
   :hit :low :high :r :g :b :a :dest :filter :origin :source :tint :radius :x3 :y3 :v1 :v2 :v3
   :load-asset :filled :font :size :spacing :asset :start :end :bezier :up :x-scale :y-scale
   :frame-count :animations :bones :materials :trans :frame-poses :maps

   ;; Math
   :vector2-subtract :vector2-add :vector2-scale :vector2-length
   :quaternion-from-euler :matrix-rotate-zyx :quaternion-to-matrix :quaternion-from-matrix
   :quaternion-to-euler

   ;; Globals
   :*claylib-background* :*screen-width* :*screen-height* :*target-fps* :+claylib-directory+

   ;; Misc
   :draw-fps :get-random-value :get-frame-time :measure-text :get-mouse-ray :get-ray-collision-box
   :+flag-window-resizable+ :+flag-vsync-hint+ :load-render-texture :+texture-filter-bilinear+
   :get-screen-width :get-screen-height :set-random-seed  :save-storage-value :load-storage-value
   :gen-mesh-cylinder :set-slot :load-model-from-mesh :close-window :gen-mesh-cube
   :set-window-state :clear-window-state :toggle-fullscreen :+flag-window-undecorated+
   :+flag-window-hidden+ :+flag-window-hidden+ :+flag-window-minimized+ :+flag-window-maximized+
   :+flag-window-unfocused+ :+flag-window-topmost+ :+flag-window-always-run+ :minimize-window
   :restore-window :maximize-window :+flag-window-transparent+ :+flag-fullscreen-mode+
   :+flag-window-highdpi+ :+flag-msaa-4x-hint+ :gen-image-checked :load-texture-from-image
   :+texture-filter-anisotropic-16x+ :+texture-wrap-clamp+ :get-time :wait-time :swap-screen-buffer
   :check-collision-point-rec :clear-background :export-image :load-image-from-texture
   :image-flip-vertical :image-flip-horizontal :image-crop :image-resize :image-draw
   :copy-asset-to-object :set-window-position :check-collision-recs :get-collision-rec
   :check-collision-point-circle :measure-text-ex :update-model-animation :set-material-texture
   :check-collision-boxes :check-collision-box-sphere :gen-mesh-cubicmap

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
   :gui-slider-bar :gui-progress-bar :gui-status-bar :gui-dummy-rec :gui-scroll-bar :gui-grid

   ;; Advance controls set
   :gui-list-view :gui-message-box :gui-text-input-box :gui-color-picker :gui-color-panel
   :gui-color-bar-alpha :gui-color-bar-hue

   ;; Styles loading functions
   :gui-load-style :gui-load-style-default

   ;; Icons functionality
   :gui-icon-text :gui-draw-icon :gui-get-icons :gui-get-icon-data :gui-set-icon-data :gui-set-icon-pixel
   :gui-clear-icon-pixel :gui-check-icon-pixel

   ;; Constants
   :+scrollbar+ :+border-width+ :+arrows-size+ :+slider-padding+ :+arrows-visible+ :+slider-width+
   :+listview+ :+scrollbar-side+ :+scrollbar-left-side+ :+scrollbar-right-side+ :+scrollbar-width+
   :+default+))
