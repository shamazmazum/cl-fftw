(in-package :cl-fftw/core)

(defconstant +forward+  -1
  "Designates a forward Fourier transform")

(defconstant +backward+ +1
  "Designates an inverse Fourier transform")

(define-condition fftw-error (error)
  ((message :initarg :message
            :reader  fftw-error-message))
  (:report (lambda (c s)
             (format s "FFTW error: ~a"
                     (fftw-error-message c)))))

;; Stuff used internally by single- and double-float parts

(defmacro def-copier-to-foreign (name type)
  `(progn
     (serapeum:-> ,name ((simple-array ,type)
                         (simple-array ,type (*)))
                  (values &optional))
     (defun ,name (src dst)
       (declare (optimize (speed 3)))
       (replace dst (sb-ext:array-storage-vector src))
       (values))))

(defmacro def-copier-from-foreign (name type)
  `(progn
     (serapeum:-> ,name ((simple-array ,type (*))
                         (simple-array ,type))
                  (values &optional))
     (defun ,name (src dst)
       (declare (optimize (speed 3)))
       (replace (sb-ext:array-storage-vector dst) src)
       (values))))

(defmacro with-pointers-to-vector-data (bindings &body body)
  (reduce
   (lambda (binding acc)
     (destructuring-bind (ptr vector) binding
       `(cffi:with-pointer-to-vector-data (,ptr ,vector)
          ,acc)))
   bindings
   :from-end t
   :initial-value `(progn ,@body)))

(defconstant +fftw-estimate+ 64)

(declaim (inline fill-dim-array))
(defun fill-dim-array (dimptr rank dimensions)
  (loop for i below rank
        for d in dimensions
        do (setf (cffi:mem-aref dimptr :int i) d)))

(defun rfft-dimensions (dimensions)
  (reduce
   (lambda (dim acc)
     (cons
      (if acc dim (1+ (floor dim 2))) acc))
   dimensions
   :from-end t
   :initial-value nil))

(declaim (inline make-array-type))
(defun make-array-type (type dimensions)
  (sb-kernel:make-array-type
   dimensions
   :complexp                 nil
   :element-type             type
   :specialized-element-type type))

(declaim (inline rfft-dimensions-for-type))
(defun rfft-dimensions-for-type (dims)
  (if (atom dims) dims
      (reduce
       (lambda (dim acc)
         (cons
          (cond
            (acc dim)
            ((integerp dim)
             (1+ (floor dim 2)))
            (t dim))
          acc))
       dims
       :from-end t
       :initial-value nil)))

(declaim (inline irfft-dimensions-for-type))
(defun irfft-dimensions-for-type (dims)
  (if (atom dims) dims
      (reduce
       (lambda (dim acc)
         (cons (if acc dim '*) acc))
       dims
       :from-end t
       :initial-value nil)))

(declaim (inline type-deriver/with-plan))
(defun type-deriver/with-plan (f)
  (let ((type (sb-c::lvar-fun-type f t t)))
    (when (sb-kernel:fun-type-p type)
      (sb-kernel:fun-type-returns type))))
