(in-package :cl-fftw/single)

;; Need to define plan handles here
(sera:defconstructor fft-plan "FFT plan handle"
  (obj  t)
  (dims list)
  (src  (simple-array (complex single-float) (*)))
  (dst  (simple-array (complex single-float) (*))))

(sera:defconstructor rfft-plan "RFFT plan handle"
  (obj         t)
  (input-dims  list)
  (output-dims list)
  (src         (simple-array          single-float  (*)))
  (dst         (simple-array (complex single-float) (*))))

(sera:defconstructor irfft-plan "IRFFT plan handle"
  (obj  t)
  (dims list)
  (src  (simple-array (complex single-float) (*)))
  (dst  (simple-array          single-float  (*))))

(sb-c:defknown %with-fft-plan (list (member -1 +1) (function (fft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)

(sb-c:defknown %with-rfft-plan (list (function (rfft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)

(sb-c:defknown rfft (rfft-plan (simple-array single-float))
    (simple-array (complex single-float))
    (sb-c:any)
  :overwrite-fndb-silently t)

(sb-c:defknown %with-irfft-plan (list (function (irfft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)
