(defpackage cl-fftw/double
  (:use #:cl #:cffi)
  (:local-nicknames (#:sera #:serapeum)
                    (#:core #:cl-fftw/core))
  (:export #:fft-plan
           #:with-fft-plan
           #:fft #:%fft
           #:with-rfft-plan
           #:rfft #:%rfft
           #:with-irfft-plan
           #:irfft #:%irfft))
