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

(declaim (inline %copy-to-foreign))
(defun %copy-to-foreign (src dst)
  (unless (= (array-total-size src) (length dst))
    (error "SRC and DST have different sizes"))
  (replace dst (sb-ext:array-storage-vector src))
  (values))

(defmacro def-copier-to-foreign (name type)
  `(progn
     (serapeum:-> ,name ((simple-array ,type)
                         (simple-array ,type (*)))
                  (values &optional))
     (defun ,name (src dst)
       (declare (optimize (speed 3)))
       (%copy-to-foreign src dst))))

(declaim (inline %copy-from-foreign))
(defun %copy-from-foreign (src dst)
  (unless (= (array-total-size dst)
             (length src))
    (error "SRC and DST have different sizes"))
  (replace (sb-ext:array-storage-vector dst) src)
  (values))

(defmacro def-copier-from-foreign (name type)
  `(progn
     (serapeum:-> ,name ((simple-array ,type (*))
                         (simple-array ,type))
                  (values &optional))
     (defun ,name (src dst)
       (declare (optimize (speed 3)))
       (%copy-from-foreign src dst))))
