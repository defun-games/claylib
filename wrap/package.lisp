(defpackage #:claylib/wrap
  (:use #:cl)
  (:export
   ;; Most symbols are exported automatically. Symbols we have to list below
   ;; are those that for some reason weren't exported or read properly.
   
   ;; Colors
   :+lightgray+ :+gray+ :+darkgray+ :+yellow+ :+gold+ :+orange+ :+pink+ :+red+ :+maroon+ :+green+
   :+lime+ :+darkgreen+ :+skyblue+ :+blue+ :+darkblue+ :+purple+ :+violet+ :+darkpurple+ :+beige+
   :+brown+ :+darkbrown+ :+white+ :+black+ :+blank+ :+magenta+ :+raywhite+

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

   ;; Raygui
   :+raygui-colorbaralpha-checked-size+ :+raygui-grid-alpha+ :+raygui-groupbox-line-thick+
   :+raygui-icon-data-elements+ :+raygui-icon-max-icons+ :+raygui-icon-max-name-length+
   :+raygui-icon-size+ :+raygui-line-margin-text+ :+raygui-line-text-padding+ :+raygui-max-controls+
   :+raygui-max-props-base+ :+raygui-max-props-extended+ :+raygui-messagebox-button-height+
   :+raygui-messagebox-button-padding+ :+raygui-panel-border-width+ :+raygui-textinputbox-button-height+
   :+raygui-textinputbox-button-padding+ :+raygui-textinputbox-height+ :+raygui-textsplit-max-items+
   :+raygui-textsplit-max-text-size+ :+raygui-togglegroup-max-items+ :+raygui-valuebox-max-chars+
   :+raygui-version+ :+raygui-windowbox-statusbar-height+))
