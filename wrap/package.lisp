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
   :+mouse-button-extra+ :+mouse-button-forward+ :+mouse-button-back+))
