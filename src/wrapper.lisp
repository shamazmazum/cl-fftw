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

(defcfun ("fft" %fft) :void
  (plan :pointer)
  (in   (:pointer :double))
  (out  (:pointer :double)))

(defcfun ("rfft" %rfft) :void
  (plan :pointer)
  (in   (:pointer :double))
  (out  (:pointer :double)))

(defcfun ("irfft" %irfft) :void
  (plan :pointer)
  (in   (:pointer :double))
  (out  (:pointer :double)))

(sera:defconstructor plan
  (obj        t)
  (dimensions list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ll-name (symbol)
    (intern (concatenate 'string "%" (symbol-name symbol)))))

(defmacro def-create-plan (name documentation bidi)
  (let ((ll-name (ll-name name)))
    `(progn
       (sera:-> ,name (list ,@(if bidi `((member -1 1))))
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
      "Create a plan for FFT transform (complex input). Sign is -1 for
forward transform and 1 for inverse transform. The plan must be
destroyed later with DESTROY-PLAN." t)

(def-create-plan create-rfft-plan
    "Create a plan for the real-input forward FFT transform (RFFT). The
plan must be destroyed later with DESTROY-PLAN." nil)

(def-create-plan create-irfft-plan
    "Create a plan for the real-input inverse FFT transform (IRFFT). The
plan must be destroyed later with DESTROY-PLAN." nil)

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
  (let ((ll-name (ll-name name)))
    `(progn
       (sera:-> ,name (plan (simple-array ,input-elt-type))
                (values (simple-array ,output-elt-type) &optional))
       (defun ,name (plan array)
         ,documentation
         (check-dimensions plan array ,@cd-args)
         (let ((result (make-array (,dim-transform (plan-dimensions plan))
                                   :element-type ',output-elt-type)))
           (with-pointer-to-vector-data (input array)
             (with-pointer-to-vector-data (output result)
               (,ll-name (plan-obj plan) input output)))
           result)))))

(def-fft (fft (complex double-float) (complex double-float) identity ())
    "Perform FFT transform. ARRAY's and PLAN's dimensions must be the
same.")

(def-fft (rfft double-float (complex double-float) rfft-dimensions ())
    "Perform RFFT transform. ARRAY's and PLAN's dimensions must be the
same.")

(def-fft (irfft (complex double-float) double-float identity (:irfft t))
    "Perform IRFFT transform. ARRAY's and PLAN's dimensions must be
compatible.")

(defmacro with-plan ((plan constructor dimensions &optional sign) &body body)
  "Create an FFT plan with CONSTRUCTOR and make sure it is destroyed
when control leaves BODY."
  `(let ((,plan (,constructor ,dimensions ,@(if sign (list sign)))))
     (unwind-protect
          (progn ,@body)
       (destroy-plan ,plan))))
