(in-package :cl-fftw/single)

(sb-c:defoptimizer (%with-fft-plan sb-c:derive-type) ((dimensions sign f))
  (declare (ignore dimensions sign))
  (let ((type (sb-c::lvar-fun-type f t t)))
    (when (sb-kernel:fun-type-p type)
      (sb-kernel:fun-type-returns type))))
