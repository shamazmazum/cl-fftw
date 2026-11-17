(defpackage cl-fftw/core
  (:use #:cl)
  (:export #:+forward+
           #:+backward+
           #:fftw-error
           ;; Utility stuff
           #:+fftw-estimate+
           #:fill-dim-array
           #:rfft-dimensions
           #:make-array-type
           #:rfft-dimensions-for-type
           #:irfft-dimensions-for-type
           #:type-deriver/with-plan
           #:with-pointers-to-vector-data
           #:def-copier-to-foreign
           #:def-copier-from-foreign))
