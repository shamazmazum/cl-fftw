(in-package :cl-fftw/single)

(sb-c:defoptimizer (%with-fft-plan sb-c:derive-type) ((dimensions sign f))
  (declare (ignore dimensions sign))
  (let ((type (sb-c::lvar-fun-type f t t)))
    (when (sb-kernel:fun-type-p type)
      (sb-kernel:fun-type-returns type))))

(sb-c:defoptimizer (%with-rfft-plan sb-c:derive-type) ((dimensions f))
  (declare (ignore dimensions))
  (let ((type (sb-c::lvar-fun-type f t t)))
    (when (sb-kernel:fun-type-p type)
      (sb-kernel:fun-type-returns type))))

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

(sb-c:defoptimizer (rfft sb-c:derive-type) ((plan array))
  (declare (ignore plan))
  (let ((dimensions (sb-c::array-type-dimensions-or-give-up
                     (sb-c::lvar-type array) nil)))
    (make-array-type (sb-kernel:specifier-type '(complex single-float))
                     (rfft-dimensions-for-type dimensions))))
