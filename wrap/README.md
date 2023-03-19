Quick instructions for regenerating claylib/wrap bindings. More about claw at https://github.com/borodust/claw.

0. Delete or move the claylib/wrap/bindings directory.
1. `(pushnew :claw-regen-adapter *features*)`
2. `(ql:quickload :cffi)`
3. `(cffi:load-foreign-library "<path-to>/claylib/wrap/libresect.so")`
4. `(ql:quickload :claylib/makewrap)`
5. `(claw:load-wrapper :claylib/makewrap)` ; May take a few minutes.
If anything was changed, you'll need to recompile the shim as well (instructions for Linux only):
6. `pushd <path-to>/claylib/wrap/lib && gcc -o librayshim.x86_64-pc-linux-gnu.so adapter.x86_64-pc-linux-gnu.c -shared -fpic -I./`

You may now proceed to `(ql:quickload :claylib/wrap)`, etc.
