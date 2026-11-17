(in-package :cl-fftw/double)

;; Copiers
(core:def-copier-to-foreign   copy-to-foreign   double-float)
(core:def-copier-from-foreign copy-from-foreign double-float)
(core:def-copier-to-foreign   copy-complex-to-foreign   (complex double-float))
(core:def-copier-from-foreign copy-complex-from-foreign (complex double-float))

(define-foreign-library libfftw3
  (:darwin (:or "libfftw3.dylib"))
  (:unix  (:or "libfftw3.so.3" "libfftw3.so"))
  (:windows (:or "libfftw3.dll"))
  (t (:default "fftw3")))

(use-foreign-library libfftw3)

;; FFT
(defcfun ("fftw_plan_dft" %%plan-fft) :pointer
  (rank       :int)
  (dimensions (:pointer :int))
  (in         :pointer)
  (out        :pointer)
  (sign       :int)
  (flags      :uint))

(defcfun ("fftw_destroy_plan" %%destroy-plan) :void
  (plan :pointer))

(defcfun ("fftw_execute" %execute) :void
  (plan :pointer))

(sera:-> %plan-fft (list (member -1 +1) t t)
         (values t &optional))
(defun %plan-fft (dimensions sign inptr outptr)
  (declare (optimize (speed 3)))
  (let ((rank (length dimensions)))
    (with-foreign-object (dimptr :int rank)
      (core:fill-dim-array dimptr rank dimensions)
      (let ((plan (%%plan-fft rank dimptr inptr outptr sign core:+fftw-estimate+)))
        (when (null-pointer-p plan)
          (error 'core:fftw-error :message "Cannot create FFT plan"))
        plan))))

(sera:-> %destroy-fft-plan (fft-plan)
         (values &optional))
(defun %destroy-fft-plan (plan)
  (declare (optimize (speed 3)))
  (%%destroy-plan
   (fft-plan-obj plan))
  (values))

(defun %with-fft-plan (dimensions sign f)
  (declare (optimize (speed 3)))
  (let* ((size (reduce #'* dimensions))
         (input  (make-array size :element-type '(complex double-float)))
         (output (make-array size :element-type '(complex double-float))))
    (declare (type integer size))
    (core:with-pointers-to-vector-data ((inptr input)
                                        (outptr output))
      (let ((plan (fft-plan (%plan-fft dimensions sign inptr outptr)
                            dimensions input output)))
        (unwind-protect
             (funcall f plan)
          (%destroy-fft-plan plan))))))

(defmacro with-fft-plan ((plan dimensions sign) &body body)
  "Create an FFT plan and make sure it is destroyed when control
leaves @c(body)."
  `(%with-fft-plan
    ,dimensions ,sign
    (lambda (,plan)
      ,@body)))

(defun fft (plan array)
  "Perform FFT transform. @c(array)'s and @c(plan)'s dimensions must
be the same."
  (declare (optimize (speed 3)))
  (unless (equalp (array-dimensions array)
                  (fft-plan-dims plan))
    (error 'core:fftw-error :message "Array and plan have different dimensions"))
  (let ((result (make-array (array-dimensions array)
                            :element-type '(complex double-float))))
    (copy-complex-to-foreign array (fft-plan-src plan))
    (%execute (fft-plan-obj plan))
    (copy-complex-from-foreign (fft-plan-dst plan) result)
    result))

(sera:-> %fft ((simple-array (complex double-float)) (member -1 +1))
         (values (simple-array (complex double-float)) &optional))
(declaim (inline %fft))
(defun %fft (array sign)
  "Perform FFT transform. This is a version of FFT without previously
created plan."
  (with-fft-plan (plan (array-dimensions array) sign)
    (fft plan array)))

;; RFFT
(defcfun ("fftw_plan_dft_r2c" %%plan-rfft) :pointer
  (rank  :int)
  (dims  (:pointer :int))
  (in    (:pointer :float))
  (out   (:pointer :float))
  (flags :uint))

(sera:-> %plan-rfft (list t t)
         (values t &optional))
(defun %plan-rfft (dimensions inptr outptr)
  (declare (optimize (speed 3)))
  (let ((rank (length dimensions)))
    (with-foreign-object (dimptr :int rank)
      (core:fill-dim-array dimptr rank dimensions)
      (let ((plan (%%plan-rfft rank dimptr inptr outptr core:+fftw-estimate+)))
        (when (null-pointer-p plan)
          (error 'core:fftw-error :message "Cannot create RFFT plan"))
        plan))))

(sera:-> %destroy-rfft-plan (rfft-plan)
         (values &optional))
(defun %destroy-rfft-plan (plan)
  (declare (optimize (speed 3)))
  (%%destroy-plan
   (rfft-plan-obj plan))
  (values))

(defun %with-rfft-plan (input-dimensions f)
  (declare (optimize (speed 3)))
  (let* ((output-dimensions (core:rfft-dimensions input-dimensions))
         (input-size  (reduce #'* input-dimensions))
         (output-size (reduce #'* output-dimensions))
         (input  (make-array input-size  :element-type 'double-float))
         (output (make-array output-size :element-type '(complex double-float))))
    (declare (type integer input-size output-size))
    (core:with-pointers-to-vector-data ((inptr input)
                                        (outptr output))
      (let ((plan (rfft-plan (%plan-rfft input-dimensions inptr outptr)
                             input-dimensions output-dimensions input output)))
        (unwind-protect
             (funcall f plan)
          (%destroy-rfft-plan plan))))))

(defmacro with-rfft-plan ((plan dimensions) &body body)
  "Create an RFFT (real-valued input) plan and make sure it is
destroyed when control leaves @c(body)."
  `(%with-rfft-plan
    ,dimensions
    (lambda (,plan)
      ,@body)))

(defun rfft (plan array)
  "Perform RFFT (real-valued input) transform. @c(array)'s and
@c(plan)'s dimensions must be the same."
  (declare (optimize (speed 3)))
  (unless (equalp (array-dimensions array)
                  (rfft-plan-input-dims plan))
    (error 'core:fftw-error :message "Array and plan have different dimensions"))
  (let ((result (make-array (rfft-plan-output-dims plan)
                            :element-type '(complex double-float))))
    (copy-to-foreign array (rfft-plan-src plan))
    (%execute (rfft-plan-obj plan))
    (copy-complex-from-foreign (rfft-plan-dst plan) result)
    result))

(sera:-> %rfft ((simple-array double-float))
         (values (simple-array (complex double-float)) &optional))
(declaim (inline %rfft))
(defun %rfft (array)
  "Perform RFFT (real-valued input) transform. This is a version of
RFFT without previously created plan."
  (with-rfft-plan (plan (array-dimensions array))
    (rfft plan array)))

;; IRFFT
(defcfun ("fftw_plan_dft_c2r" %%plan-irfft) :pointer
  (rank  :int)
  (dims  (:pointer :int))
  (in    (:pointer :float))
  (out   (:pointer :float))
  (flags :uint))

(sera:-> %plan-irfft (list t t)
         (values t &optional))
(defun %plan-irfft (dimensions inptr outptr)
  (declare (optimize (speed 3)))
  (let ((rank (length dimensions)))
    (with-foreign-object (dimptr :int rank)
      (core:fill-dim-array dimptr rank dimensions)
      (let ((plan (%%plan-irfft rank dimptr inptr outptr core:+fftw-estimate+)))
        (when (null-pointer-p plan)
          (error 'core:fftw-error :message "Cannot create IRFFT plan"))
        plan))))

(sera:-> %destroy-irfft-plan (irfft-plan)
         (values &optional))
(defun %destroy-irfft-plan (plan)
  (declare (optimize (speed 3)))
  (%%destroy-plan
   (irfft-plan-obj plan))
  (values))

(defun %with-irfft-plan (real-dimensions f)
  (declare (optimize (speed 3)))
  (let* ((complex-dimensions (core:rfft-dimensions real-dimensions))
         (real-size    (reduce #'* real-dimensions))
         (complex-size (reduce #'* complex-dimensions))
         (input  (make-array complex-size :element-type '(complex double-float)))
         (output (make-array real-size    :element-type 'double-float)))
    (declare (type integer real-size complex-size))
    (core:with-pointers-to-vector-data ((inptr input)
                                        (outptr output))
      (let ((plan (irfft-plan (%plan-irfft real-dimensions inptr outptr)
                              real-dimensions complex-dimensions input output)))
        (unwind-protect
             (funcall f plan)
          (%destroy-irfft-plan plan))))))

(defmacro with-irfft-plan ((plan dimensions) &body body)
  "Create an IRFFT (real-valued input inverse FFT) plan and make sure
it is destroyed when control leaves @c(body)."
  `(%with-irfft-plan
    ,dimensions
    (lambda (,plan)
      ,@body)))

(defun irfft (plan array)
  "Perform IRFFT (real-valued input inverse FFT)
transform. @c(array)'s dimensions must be compatible with @c(plan)'s
dimensions."
  (declare (optimize (speed 3)))
  (unless (equalp (array-dimensions array)
                  (irfft-plan-complex-dims plan))
    (error 'core:fftw-error :message "Array and plan have different dimensions"))
  (let ((result (make-array (irfft-plan-real-dims plan)
                            :element-type 'double-float)))
    (copy-complex-to-foreign array (irfft-plan-src plan))
    (%execute (irfft-plan-obj plan))
    (copy-from-foreign (irfft-plan-dst plan) result)
    result))

(sera:-> %irfft ((simple-array (complex double-float)) list)
         (values (simple-array double-float) &optional))
(declaim (inline %irfft))
(defun %irfft (array dimensions)
  "Perform IRFFT (real-valued input inverse FFT) transform. This is a
version of IRFFT without previously created plan."
  (with-irfft-plan (plan dimensions)
    (irfft plan array)))
