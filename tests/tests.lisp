(in-package :cl-fftw/tests)

(def-suite one-dim/double :description "One dimensional transforms (double precision)")
(def-suite props/double :description "Generic FFT properties (double precision)")
(def-suite props/single :description "Generic FFT properties (single precision)")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(one-dim/double props/double props/single))))

(defun random-content (dimensions type &key complexp)
  (let ((const (ecase type
                 (single-float 1f0)
                 (double-float 1d0))))
    (labels ((%go (dimensions)
               (if (null (cdr dimensions))
                   (loop repeat (car dimensions) collect
                         (if complexp
                             (complex (random const)
                                      (random const))
                             (random const)))
                   (loop repeat (car dimensions) collect
                         (%go (cdr dimensions))))))
      (make-array dimensions
                  :element-type (if complexp (list 'complex type) type)
                  :initial-contents (%go dimensions)))))


(in-suite one-dim/double)
(test complex/yaft/double
  (loop repeat 1000
        for length = (+ (random 4000) 2)
        for array  = (random-content (list length) 'double-float :complexp t)
        for fft1 = (yaft:fft array yaft:+forward+)
        for fft2 = (cl-fftw/double:%fft array cl-fftw/core:+forward+)
        do (is-true (approx:array-approx-p fft1 fft2))))

(test real/yaft/double
  (loop repeat 1000
        for length = (* (+ (random 4000) 2) 2)
        for array  = (random-content (list length) 'double-float :complexp nil)
        for fft1   = (yaft:rfft array)
        for fft2   = (cl-fftw/double:%rfft array)
        do (is-true (approx:array-approx-p fft1 fft2))))

(in-suite props/double)
(test fft/double
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 100)))
        for array = (random-content dims 'double-float :complexp t)
        for total = (array-total-size array)
        for forward = (cl-fftw/double:%fft array   cl-fftw/core:+forward+)
        for inverse = (cl-fftw/double:%fft forward cl-fftw/core:+backward+) do
        (is (approx:approxp (row-major-aref forward 0) (reduce #'+ (aops:flatten array))))
        (map-into (aops:flatten array)
                  (lambda (x) (* x total))
                  (aops:flatten array))
        (is-true (approx:array-approx-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))

(test rfft/double
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 100)))
        for array = (random-content dims 'double-float :complexp nil)
        for total = (array-total-size array)
        for forward = (cl-fftw/double:%rfft array)
        for inverse = (cl-fftw/double:%irfft forward dims) do
        (is (approx:approxp (row-major-aref forward 0) (reduce #'+ (aops:flatten array))))
        (map-into (aops:flatten array)
                  (lambda (x) (* x total))
                  (aops:flatten array))
        (is-true (approx:array-approx-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))

(in-suite props/single)
(test fft/single
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 10)))
        for array = (random-content dims 'single-float :complexp t)
        for total = (array-total-size array)
        for forward = (cl-fftw/single:%fft array   cl-fftw/core:+forward+)
        for inverse = (cl-fftw/single:%fft forward cl-fftw/core:+backward+) do
        (is (approx:approxp (row-major-aref forward 0) (reduce #'+ (aops:flatten array))))
        (map-into (aops:flatten array)
                  (lambda (x) (* x total))
                  (aops:flatten array))
        (is-true (approx:array-approx-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))

(test rfft/single
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 10)))
        for array = (random-content dims 'single-float :complexp nil)
        for total = (array-total-size array)
        for forward = (cl-fftw/single:%rfft array)
        for inverse = (cl-fftw/single:%irfft forward dims) do
        (is (approx:approxp (row-major-aref forward 0) (reduce #'+ (aops:flatten array))))
        (map-into (aops:flatten inverse)
                  (lambda (x) (/ x total))
                  (aops:flatten inverse))
        (is-true (approx:array-approx-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))
