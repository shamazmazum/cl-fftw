(defpackage cl-fftw/single
  (:use #:cl #:cffi)
  (:local-nicknames (#:sera #:serapeum)
                    (#:core #:cl-fftw/core))
  (:export #:fft-plan
           #:with-fft-plan
           #:fft #:%fft
           #:with-rfft-plan
           #:rfft #:%rfft

   ;; OLD API
   #:plan
           #:destroy-plan
           #:create-fft-plan
           #:create-rfft-plan
           #:create-irfft-plan
           #:with-plan
           #:fft #:rfft #:irfft
           #:%fft #:%rfft #:%irfft))
