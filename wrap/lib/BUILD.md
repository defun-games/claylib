# Building your own shared libs
Disclaimer: Raylib has many configuration options and ways to build it. What follows is just _one_ configuration that has worked for us (the one we distribute). It is probably not the _only_ configuration that will work.

## Linux
### Raylib
1. See [Raylib build instructions](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux).
2. Apply our Raylib and Raymath [patches](/wrap/patches) or simply copy our raylib.h and raymath.h into the src/ directory.
3. Edit raymath.h and delete or comment out the line: `#define RAYMATH_IMPLEMENTATION`. This is a little weird, but it's because cl-autowrap requires `RAYMATH_IMPLEMENTATION` to be able to parse all the symbols, yet it's already defined elsewhere in Raylib so you get duplicate definition errors if you try to build the .so with it.
4. `cmake` is recommended:
```
cmake -DCUSTOMIZE_BUILD=ON -DCMAKE_BUILD_TYPE=MinSizeRel -DBUILD_SHARED_LIBS=ON -DUSE_EXTERNAL_GLFW=ON -DUSE_WAYLAND=ON -DWITH_PIC=ON -DOpenGL_GL_PREFERENCE=GLVND
```
5. If you are using a system GLFW _and_ your GLFW is the stable branch (not dev), you will run into an issue where Raylib expects a bleeding-edge symbol from the dev GLFW that yours doesn't have. The fix is to add this into the definitions of rcore.c:
```
#if !defined(GLFW_MOUSE_PASSTHROUGH)
    #define GLFW_MOUSE_PASSTHROUGH      0x0002000D
#endif
```
6. Follow the rest of the instructions, i.e. `make`.

### Raygui
1. See [Raygui build instructions](https://github.com/raysan5/raygui).
2. Apply our Raygui [patch](/wrap/patches) or simply copy our raygui.h into the src/ directory.
3. Make an `include` directory and copy our raylib.h into it.
4. Build like this, noting the conspicuous _lack_ of `-lraylib`. This is to prevent Raygui trying to use a Raylib .so that might not be installed on your system:
```
gcc -o libraygui.so src/raygui.c -shared -fpic -DRAYGUI_IMPLEMENTATION -lGL -lm -lpthread -ldl -lrt -I./include
```

## Other Platforms
... haven't been tested yet. See the build instructions [here](https://github.com/raysan5/raylib) and [here](https://github.com/raysan5/raygui). Pull requests welcome!
