(in-package :cl-fftw/double)

;; Need to define plan handles here
(sera:defconstructor fft-plan "FFT plan handle"
  (obj  t)
  (dims list)
  (src  (simple-array (complex double-float) (*)))
  (dst  (simple-array (complex double-float) (*))))

(sera:defconstructor rfft-plan "RFFT plan handle"
  (obj         t)
  (input-dims  list)
  (output-dims list)
  (src         (simple-array          double-float  (*)))
  (dst         (simple-array (complex double-float) (*))))

(sera:defconstructor irfft-plan "IRFFT plan handle"
  (obj  t)
  (real-dims    list)
  (complex-dims list)
  (src  (simple-array (complex double-float) (*)))
  (dst  (simple-array          double-float  (*))))

(sb-c:defknown %with-fft-plan (list (member -1 +1) (function (fft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)

(sb-c:defknown fft (fft-plan (simple-array (complex double-float)))
    (simple-array (complex double-float))
    (sb-c:any)
  :overwrite-fndb-silently t)

(sb-c:defknown %with-rfft-plan (list (function (rfft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)

(sb-c:defknown rfft (rfft-plan (simple-array double-float))
    (simple-array (complex double-float))
    (sb-c:any)
  :overwrite-fndb-silently t)

(sb-c:defknown %with-irfft-plan (list (function (irfft-plan) *)) *
    (sb-c:call)
  :overwrite-fndb-silently t)

(sb-c:defknown irfft (irfft-plan (simple-array (complex double-float)))
    (simple-array double-float)
    (sb-c:any)
  :overwrite-fndb-silently t)
