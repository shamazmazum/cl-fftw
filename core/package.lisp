(defpackage cl-fftw/core
  (:use #:cl)
  (:export #:+forward+
           #:+backward+
           #:fftw-error
           ;; Utility stuff
           #:with-pointers-to-vector-data
           #:def-copier-to-foreign
           #:def-copier-from-foreign))
