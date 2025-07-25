(in-package :cl-fftw)

(let ((pathname
       (asdf:output-file
        'asdf:compile-op
        (asdf:find-component
         :cl-fftw "libfftwrap"))))
  (pushnew
   (make-pathname
    :directory (pathname-directory pathname))
   *foreign-library-directories*
   :test #'equalp))

(define-foreign-library libfftwrap
  (:darwin (:or "libfftwrap.dylib" "fftwrap.dylib"))
  (:unix  (:or "libfftwrap.so" "fftwrap.so"))
  (:windows (:or "libfftwrap.dll" "fftwrap.dll"))
  (t (:default "fftwrap")))

(use-foreign-library libfftwrap)

(defcfun ("create_fft_plan" %create-fft-plan) :pointer
  (rank       :int)
  (dimensions (:pointer :int))
  (sign       :int))

(defcfun ("create_rfft_plan" %create-rfft-plan) :pointer
  (rank       :int)
  (dimensions (:pointer :int)))

(defcfun ("create_irfft_plan" %create-irfft-plan) :pointer
  (rank       :int)
  (dimensions (:pointer :int)))

(defcfun ("destroy_plan" %destroy-plan) :void
  (plan :pointer))

(defcfun ("get_input_pointer" get-input-pointer) :pointer
  (plan :pointer))

(defcfun ("get_output_pointer" get-output-pointer) :pointer
  (plan :pointer))

(defcfun ("execute_plan" execute-plan) :pointer
  (plan :pointer))

(sera:defconstructor plan
  (obj        t)
  (dimensions list))

(declaim (inline copy-to-c!))
(defun copy-to-c! (plan array)
  (let ((ptr (get-input-pointer (plan-obj plan)))
        (type (array-element-type array)))
    (cond
      ((equalp type '(complex double-float))
       (loop for i below (array-total-size array)
             for j from 0 by 2
             for x = (row-major-aref array i) do
             (setf (mem-aref ptr :double j)
                   (realpart x)
                   (mem-aref ptr :double (1+ j))
                   (imagpart x))))
      ((equalp type 'double-float)
       (loop for i below (array-total-size array) do
             (setf (mem-aref ptr :double i)
                   (row-major-aref array i))))
      (t (error "Unreachable"))))
  array)

(declaim (inline copy-from-c!))
(defun copy-from-c! (plan array)
  (let ((ptr (get-output-pointer (plan-obj plan)))
        (type (array-element-type array)))
    (cond
      ((equalp type '(complex double-float))
       (loop for i below (array-total-size array)
             for j from 0 by 2
             for x = (complex (mem-aref ptr :double j)
                              (mem-aref ptr :double (1+ j)))
             do
             (setf (row-major-aref array i) x)))
      ((equalp type 'double-float)
       (loop for i below (array-total-size array) do
             (setf (row-major-aref array i)
                   (mem-aref ptr :double i))))
      (t (error "Unreachable"))))
  array)

(defmacro def-create-plan (name documentation bidi)
  (let ((ll-name (intern (concatenate 'string "%" (symbol-name name)))))
    `(progn
       (sera:-> ,name (list ,@(if bidi `((member -1 +1))))
                (values plan &optional))
       (defun ,name (dimensions ,@(if bidi `(sign)))
         ,documentation
         (let ((rank (length dimensions)))
           (with-foreign-object (ds :int rank)
             (loop for i below rank
                   for d in dimensions do
                   (setf (mem-aref ds :int i) d))
             (plan
              (,ll-name rank ds ,@(if bidi `(sign)))
              dimensions)))))))

(def-create-plan create-fft-plan
    "Create a plan for FFT transform (complex input). Sign is
@c(+forward+)(-1) for forward transform and @c(+backward)(+1) for
inverse transform. The plan must be destroyed later with
@c(destroy-plan)." t)

(def-create-plan create-rfft-plan
    "Create a plan for the real-input forward FFT transform (RFFT). The
plan must be destroyed later with @c(destroy-plan)." nil)

(def-create-plan create-irfft-plan
    "Create a plan for the real-input inverse FFT transform (IRFFT). The
plan must be destroyed later with @c(destroy-plan)." nil)

(sera:-> destroy-plan (plan)
         (values &optional))
(defun destroy-plan (plan)
  "Destroy a FFT/RFFT/IRFFT plan."
  (%destroy-plan
   (plan-obj plan)))

(defun rfft-dimensions (dimensions)
  (reduce
   (lambda (dim acc)
     (cons
      (if acc dim (1+ (floor dim 2))) acc))
   dimensions
   :from-end t
   :initial-value nil))

(declaim (inline check-dimensions))
(defun check-dimensions (plan array &key irfft)
  (unless (equalp (array-dimensions array)
                  (funcall
                   (if irfft #'rfft-dimensions #'identity)
                   (plan-dimensions plan)))
    (error "Dimension mismatch, check your plan")))

(defmacro def-fft ((name input-elt-type output-elt-type dim-transform cd-args)
                   documentation)
  `(progn
     (sera:-> ,name (plan (simple-array ,input-elt-type))
              (values (simple-array ,output-elt-type) &optional))
     (defun ,name (plan array)
       ,documentation
       (check-dimensions plan array ,@cd-args)
       (let ((result (make-array (,dim-transform (plan-dimensions plan))
                                 :element-type ',output-elt-type)))
         (copy-to-c! plan array)
         (execute-plan (plan-obj plan))
         (copy-from-c! plan result)))))

(def-fft (fft (complex double-float) (complex double-float) identity ())
    "Perform FFT transform. @c(array)'s and @c(plan)'s dimensions must
be the same.")

(def-fft (rfft double-float (complex double-float) rfft-dimensions ())
    "Perform RFFT transform. @c(array)'s and @c(plan)'s dimensions must
be the same.")

(def-fft (irfft (complex double-float) double-float identity (:irfft t))
    "Perform IRFFT transform. @c(array)'s and @c(plan)'s dimensions must
be compatible.")

(defmacro with-plan ((plan constructor dimensions &optional sign) &body body)
  "Create an FFT plan with @c(constructor) and make sure it is destroyed
when control leaves @c(body)."
  `(let ((,plan (,constructor ,dimensions ,@(if sign (list sign)))))
     (unwind-protect
          (progn ,@body)
       (destroy-plan ,plan))))

(sera:-> %fft ((simple-array (complex double-float)) (member -1 +1))
         (values (simple-array (complex double-float)) &optional))
(defun %fft (array sign)
  "Perform FFT transform. This is a version of FFT without previously
created plan."
  (with-plan (plan create-fft-plan (array-dimensions array) sign)
    (fft plan array)))

(sera:-> %rfft ((simple-array double-float))
         (values (simple-array (complex double-float)) &optional))
(defun %rfft (array)
  "Perform RFFT transform. This is a version of RFFT without
previously created plan."
  (with-plan (plan create-rfft-plan (array-dimensions array))
    (rfft plan array)))

(sera:-> %irfft ((simple-array (complex double-float)) list)
         (values (simple-array double-float) &optional))
(defun %irfft (array dimensions)
  "Perform IRFFT transform. This is a version of IRFFT without
previously created plan. @c(dimensions) is a list of dimensions of the
result."
  (with-plan (plan create-irfft-plan dimensions)
    (irfft plan array)))

(defconstant +forward+  -1)
(defconstant +backward+ +1)
