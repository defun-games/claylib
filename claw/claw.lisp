(claw:defwrapper (:claylib/makeshim
                  (:headers "raylib.h" "raymath.h" "raygui.h")
                  (:includes :raylib-includes)
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86 :linux) "i686-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-pc-windows-msvc")
                            ((:and :x86-64 :windows) "i686-pc-windows-msvc")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu")
                            ((:and :x86-64 :darwin) "i686-apple-darwin-gnu"))
                  (:persistent :claylib/wrap
                   :asd-path "claylib-shim.asd"
                   :bindings-path "bindings/shim/")
                  (:include-definitions ".*")
                  (:exclude-definitions "^__"
                                        #|"LIGHTGRAY"
                                        "GRAY"
                                        "DARKGRAY"
                                        "YELLOW"
                                        "GOLD"
                                        "ORANGE"
                                        "PINK"
                                        "RED"
                                        "MAROON"
                                        "GREEN"
                                        "LIME"
                                        "DARKGREEN"
                                        "SKYBLUE"
                                        "BLUE"
                                        "DARKBLUE"
                                        "PURPLE"
                                        "VIOLET"
                                        "DARKPURPLE"
                                        "BEIGE"
                                        "BROWN"
                                        "DARKBROWN"
                                        "WHITE"
                                        "BLACK"
                                        "BLANK"
                                        "MAGENTA"
                                        "RAYWHITE"|#))
  :in-package :claylib/wrap
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :recognize-arrays t
  :with-adapter (:dynamic :path "lib/adapter.c")
#|  :symbolicate-names (:in-pipeline
                      (:by-changing "asin" "C-ASIN")
                      (:by-changing "stream" "C-STREAM")
                      (:by-changing "log" "C-LOG")
                      (:by-changing "count" "C-COUNT")
                      (:by-changing "tan" "C-TAN")
                      (:by-changing "max" "C-MAX")
                      (:by-changing "asinh" "C-ASINH")
                      (:by-changing "cos" "C-COS")
                      (:by-changing "format" "C-FORMAT")
                      (:by-changing "acosh" "C-ACOSH")
                      (:by-changing "sqrt" "C-SQRT")
                      (:by-changing "cosh" "C-COSH")
                      (:by-changing "floor" "C-FLOOR")
                      (:by-changing "sin" "C-SIN")
                      (:by-changing "atanh" "C-ATANH")
                      (:by-changing "atan" "C-ATAN")
                      (:by-changing "position" "C-POSITION")
                      (:by-changing "round" "C-ROUND")
                      (:by-changing "exp" "C-EXP")
                      (:by-changing "min" "C-MIN")
                      (:by-changing "sinh" "C-SINH")
                      (:by-changing "tanh" "C-TANH")
                      (:by-changing "acos" "C-ACOS"))|#)

(claw:defwrapper (:claylib/makewrap
                  (:headers "adapter.x86_64-pc-linux-gnu.c")
                  (:includes :raylib-includes)
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86 :linux) "i686-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-pc-windows-msvc")
                            ((:and :x86-64 :windows) "i686-pc-windows-msvc")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu")
                            ((:and :x86-64 :darwin) "i686-apple-darwin-gnu"))
                  (:persistent :claylib/wrap
                   :asd-path "claylib-wrap.asd"
                   :bindings-path "bindings/")
                  (:include-definitions ".*")
                  (:exclude-definitions "_t$"
                                        "^__(?!claw)"
                                        "LIGHTGRAY"
                                        "GRAY"
                                        "DARKGRAY"
                                        "YELLOW"
                                        "GOLD"
                                        "ORANGE"
                                        "PINK"
                                        "RED"
                                        "MAROON"
                                        "GREEN"
                                        "LIME"
                                        "DARKGREEN"
                                        "SKYBLUE"
                                        "BLUE"
                                        "DARKBLUE"
                                        "PURPLE"
                                        "VIOLET"
                                        "DARKPURPLE"
                                        "BEIGE"
                                        "BROWN"
                                        "DARKBROWN"
                                        "WHITE"
                                        "BLACK"
                                        "BLANK"
                                        "MAGENTA"
                                        "RAYWHITE"))
  :in-package :claylib/wrap
  :trim-enum-prefix t
  :recognize-bitfields t
  :recognize-strings t
  :recognize-arrays t
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "__claw_cE3AE40FE40" "__claw_")
                      (:by-removing-postfixes "_")
                      (:by-replacing "^(Vector[23])([A-Z])" "\\1-\\2")
                      (:by-changing "asin" "C-ASIN")
                      (:by-changing "log" "C-LOG")
                      (:by-changing "tan" "C-TAN")
                      (:by-changing "asinh" "C-ASINH")
                      (:by-changing "cos" "C-COS")
                      (:by-changing "acosh" "C-ACOSH")
                      (:by-changing "sqrt" "C-SQRT")
                      (:by-changing "cosh" "C-COSH")
                      (:by-changing "floor" "C-FLOOR")
                      (:by-changing "sin" "C-SIN")
                      (:by-changing "atanh" "C-ATANH")
                      (:by-changing "atan" "C-ATAN")
                      (:by-changing "round" "C-ROUND")
                      (:by-changing "exp" "C-EXP")
                      (:by-changing "sinh" "C-SINH")
                      (:by-changing "tanh" "C-TANH")
                      (:by-changing "acos" "C-ACOS"))
  #|
  :ignore-entities #'(lambda (sym)
                       (cl-ppcre:scan "^__(?!claw)" (slot-value sym 'claw.spec::name)))|#)
