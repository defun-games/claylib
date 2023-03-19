# Building your own shared libs
Disclaimer: Raylib has many configuration options and ways to build it. What follows is just _one_ configuration that has worked for us (the one we distribute). It is probably not the _only_ configuration that will work.

## Linux
### Raylib
1. See [Raylib build instructions](https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux).
2. `cmake` is recommended:
```
cmake -DCUSTOMIZE_BUILD=ON -DCMAKE_BUILD_TYPE=MinSizeRel -DBUILD_SHARED_LIBS=ON -DUSE_EXTERNAL_GLFW=ON -DUSE_WAYLAND=ON -DWITH_PIC=ON -DOpenGL_GL_PREFERENCE=GLVND
```
3. If you are using a system GLFW _and_ your GLFW is the stable branch (not dev), you will run into an issue where Raylib expects a bleeding-edge symbol from the dev GLFW that yours doesn't have. The fix is to add this into the definitions of rcore.c:
```
#if !defined(GLFW_MOUSE_PASSTHROUGH)
    #define GLFW_MOUSE_PASSTHROUGH      0x0002000D
#endif
```
4. Follow the rest of the instructions, i.e. `make`.

### Raygui
1. See [Raygui build instructions](https://github.com/raysan5/raygui).
2. Make an `include` directory and copy raylib.h into it.
3. Build like this, noting the conspicuous _lack_ of `-lraylib`. This is to prevent Raygui trying to use a Raylib .so that might not be installed on your system:
```
gcc -o libraygui.so src/raygui.c -shared -fpic -DRAYGUI_IMPLEMENTATION -lGL -lm -lpthread -ldl -lrt -I./include
```

## Other Platforms
... haven't been tested yet. See the build instructions [here](https://github.com/raysan5/raylib) and [here](https://github.com/raysan5/raygui). Pull requests welcome!
