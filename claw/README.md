Quick instructions for regenerating claylib/wrap bindings. More about claw at https://github.com/borodust/claw.

0. Delete or move the claylib/claw/bindings directory.
1. `(pushnew :claw-regen-adapter *features*)`
2. `(ql:quickload :cffi)`
3. `(cffi:load-foreign-library "<path-to>/claylib/claw/libresect.so")`
4. `(ql:quickload :claylib/makewrap)`
5. `(claw:load-wrapper :claylib/makewrap)  ; This may take a minute depending on your machine.`
6. You may now proceed to `(ql:quickload :claylib/wrap)`, etc.
