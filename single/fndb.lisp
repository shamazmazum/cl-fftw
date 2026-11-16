(in-package :cl-fftw/single)

;; Need to define plan handles here
(sera:defconstructor fft-plan
  "FFT plan handle"
  (obj  t)
  (dims list)
  (src  (simple-array (complex single-float) (*)))
  (dst  (simple-array (complex single-float) (*))))

(sb-c:defknown %with-fft-plan (list (member -1 +1) (function (fft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)
