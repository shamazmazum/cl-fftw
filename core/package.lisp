(defpackage cl-fftw/core
  (:use #:cl)
  (:export #:+forward+
           #:+backward+
           #:fftw-error
           ;; Utility stuff
           #:def-copier-to-foreign
           #:def-copier-from-foreign))
