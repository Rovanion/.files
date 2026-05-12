;; This command asks the compiler to resolve native for your current CPU
;; and display the resulting target.
(defvar my-cpu-architecture
  (string-trim (shell-command-to-string "gcc -march=native -Q --help=target | grep -- '-march=   ' | cut -f 3")))


;; `native-comp-compiler-options' specifies flags passed directly to the C
;; compiler (for example, GCC) when compiling the Lisp-to-C output
;; produced by the native compilation process. These flags affect code
;; generation, optimization, and debugging information.
(setq native-comp-compiler-options '("-O2"
                                     "-g0"
                                     "-fno-omit-frame-pointer"
                                     "-fno-finite-math-only"))

;; `native-comp-driver-options' specifies additional flags passed to the native
;; compilation driver process, which may invoke the compiler and linker with
;; certain parameters.
(setq native-comp-driver-options `(,(format "-mtune=%s" my-cpu-architecture)
                                   ,(format "-march=%s" my-cpu-architecture)))
