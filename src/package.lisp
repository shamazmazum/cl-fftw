(defpackage cl-fftw
  (:use #:cl #:cffi)
  (:local-nicknames (#:sera #:serapeum))
  (:export #:plan
           #:destroy-plan
           #:create-fft-plan
           #:create-rfft-plan
           #:create-irfft-plan
           #:with-plan
           #:fft #:rfft #:irfft
           #:%fft #:%rfft #:%irfft
           #:+forward+
           #:+backward+))
