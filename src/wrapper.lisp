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

(defmacro def-create-plan (name documentation bidi)
  (let ((low-level-name (find-symbol (concatenate 'string "%" (symbol-name name)))))
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
              (,low-level-name rank ds ,@(if bidi `(sign)))
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

(sera:-> fft (plan (simple-array (complex double-float)))
         (values (simple-array (complex double-float)) &optional))
(defun fft (plan array)
  "Perform FFT transform. ARRAY's and PLAN's dimensions must be the
same."
  (check-dimensions plan array)
  (let ((result (make-array (array-dimensions array)
                            :element-type '(complex double-float))))
    (with-pointer-to-vector-data (input array)
      (with-pointer-to-vector-data (output result)
        (%fft (plan-obj plan) input output)))
    result))

(sera:-> rfft (plan (simple-array double-float))
         (values (simple-array (complex double-float)) &optional))
(defun rfft (plan array)
  "Perform RFFT transform. ARRAY's and PLAN's dimensions must be the
same."
  (check-dimensions plan array)
  (let ((result (make-array (rfft-dimensions (array-dimensions array))
                            :element-type '(complex double-float))))
    (with-pointer-to-vector-data (input array)
      (with-pointer-to-vector-data (output result)
        (%rfft (plan-obj plan) input output)))
    result))

(sera:-> irfft (plan (simple-array (complex double-float)))
         (values (simple-array double-float) &optional))
(defun irfft (plan array)
  "Perform IRFFT transform. ARRAY's and PLAN's dimensions must be
compatible."
  (check-dimensions plan array :irfft t)
  (let ((result (make-array (plan-dimensions plan)
                            :element-type 'double-float)))
    (with-pointer-to-vector-data (input array)
      (with-pointer-to-vector-data (output result)
        (%irfft (plan-obj plan) input output)))
    result))

(defmacro with-fft-plan ((plan dimensions sign) &body body)
  "Create an FFT plan and make sure it is destroyed when control
leaves BODY."
  `(let ((,plan (create-fft-plan ,dimensions ,sign)))
     (unwind-protect
          (progn ,@body)
       (destroy-plan ,plan))))

(defmacro with-rfft-plan ((plan dimensions) &body body)
  "Create an RFFT plan and make sure it is destroyed when control
leaves BODY."
  `(let ((,plan (create-rfft-plan ,dimensions)))
     (unwind-protect
          (progn ,@body)
       (destroy-plan ,plan))))

(defmacro with-irfft-plan ((plan dimensions) &body body)
  "Create an IRFFT plan and make sure it is destroyed when control
leaves BODY."
  `(let ((,plan (create-irfft-plan ,dimensions)))
     (unwind-protect
          (progn ,@body)
       (destroy-plan ,plan))))
