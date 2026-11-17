(in-package :cl-fftw/double)

(sb-c:defoptimizer (%with-fft-plan sb-c:derive-type) ((dimensions sign f))
  (declare (ignore dimensions sign))
  (core:type-deriver/with-plan f))

(sb-c:defoptimizer (fft sb-c:derive-type) ((plan array))
  (declare (ignore plan))
  (core:make-array-type
   (sb-kernel:specifier-type '(complex double-float))
   (sb-c::array-type-dimensions-or-give-up
    (sb-c::lvar-type array) nil)))

(sb-c:defoptimizer (%with-rfft-plan sb-c:derive-type) ((dimensions f))
  (declare (ignore dimensions))
  (core:type-deriver/with-plan f))

(sb-c:defoptimizer (rfft sb-c:derive-type) ((plan array))
  (declare (ignore plan))
  (core:make-array-type
   (sb-kernel:specifier-type '(complex double-float))
   (core:rfft-dimensions-for-type
    (sb-c::array-type-dimensions-or-give-up
     (sb-c::lvar-type array) nil))))

(sb-c:defoptimizer (%with-irfft-plan sb-c:derive-type) ((dimensions f))
  (declare (ignore dimensions))
  (core:type-deriver/with-plan f))

(sb-c:defoptimizer (irfft sb-c:derive-type) ((plan array))
  (declare (ignore plan))
  (core:make-array-type
   (sb-kernel:specifier-type 'double-float)
   (core:irfft-dimensions-for-type
    (sb-c::array-type-dimensions-or-give-up
     (sb-c::lvar-type array) nil))))
