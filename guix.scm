(use-modules (guix)
             (guix gexp)
             (guix licenses)
             (guix git-download)
             (guix build-system cmake)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages base)
             (gnu packages gl)
             (gnu packages game-development))

(define-public claylib-raylib
  (package
    (inherit raylib)
    (name "claylib-raylib")
    (source
     (origin
       (inherit (package-source raylib))
       (patches (list (local-file "./wrap/patches/raylib.patch")))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "MinSizeRel"
       #:configure-flags
       (list "-DCUSTOMIZE_BUILD=ON"
             "-DBUILD_SHARED_LIBS=ON"
             "-DUSE_EXTERNAL_GLFW=ON"
             "-DUSE_WAYLAND=ON"
             "-DWITH_PIC=ON"
             "-DOpenGL_GL_PREFERENCE=GLVND")))

    (inputs (cons (list "glfw" glfw)
                  (package-inputs raylib)))))

(define-public claylib-raygui
  (package
    (name "claylib-raygui")
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/raysan5/raygui/")
                    (commit version)))
              (file-name (git-file-name "raygui" version))
              (sha256
               (base32 "1i82xvgvikpcrsl76r5xzazbw42pr0x0lj8kmi455v92awsfc1lb"))))
    (build-system gnu-build-system)
    (inputs (list mesa))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
        (delete 'bootstrap)
        (delete 'configure)
        (replace 'build
                 (lambda _
                   (invoke "gcc"
                           "-DRAYGUI_IMPLEMENTATION"
                           "-o" "libraygui.so"
                           "-shared" "-fpic"
                           "-lGL" "-lm" "-lpthread" "-ldl" "-lrt"
                           "-I./include")))
        (delete 'check)
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (libout (string-append out "/lib/")))
                     (install-file "libraygui.so" libout)))))))
    (synopsis "C library for videogame programming")
    (description "")
    (home-page "")
    (license zlib)))

(package
 (inherit hello)
 (name "claylib")
 (version "0.0")
 (inputs (list claylib-raylib
               claylib-raygui)))
