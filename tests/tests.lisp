(in-package :cl-fftw/tests)

(def-suite one-dim :description "One dimensional transforms")
(def-suite props   :description "Generic FFT properties")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(one-dim props))))

(defun random-content (dimensions &key complexp)
  (if (null (cdr dimensions))
      (loop repeat (car dimensions) collect
            (if complexp
                (complex (random 1d0)
                         (random 1d0))
                (random 1d0)))
      (loop repeat (car dimensions) collect
            (random-content (cdr dimensions)
                            :complexp complexp))))

(defun arrays-almost-equal-p (xs ys)
  (flet ((almost-equal (x y)
           (< (- x y) 1d-8)))
    (cond
      ((and (subtypep (array-element-type xs) 'complex)
            (subtypep (array-element-type ys) 'complex))
       (every (lambda (x y)
                (and (almost-equal (realpart x)
                                   (realpart y))
                     (almost-equal (imagpart x)
                                   (imagpart y))))
              xs ys))
      ((and (subtypep (array-element-type xs) 'real)
            (subtypep (array-element-type ys) 'real))
       (every #'almost-equal xs ys)))))


(in-suite one-dim)
(test complex/yaft
  (loop repeat 1000
        for length = (+ (random 4000) 2)
        for array  = (make-array length
                                 :element-type '(complex double-float)
                                 :initial-contents (loop repeat length collect
                                                         (complex
                                                          (random 1d0)
                                                          (random 1d0))))
        for fft1 = (yaft:fft array yaft:+forward+)
        for fft2 = (cl-fftw:%fft array cl-fftw:+forward+)
        do (is-true (arrays-almost-equal-p fft1 fft2))))

(test real/yaft
  (loop repeat 1000
        for length = (* (+ (random 4000) 2) 2)
        for array  = (make-array length
                                 :element-type 'double-float
                                 :initial-contents (loop repeat length collect (random 1d0)))
        for fft1   = (yaft:rfft array)
        for fft2   = (cl-fftw:%rfft array)
        do (is-true (arrays-almost-equal-p fft1 fft2))))

(in-suite props)
(test fft
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 100)))
        for array = (make-array dims
                                :element-type '(complex double-float)
                                :initial-contents (random-content dims :complexp t))
        for total = (array-total-size array)
        for forward = (cl-fftw:%fft array   cl-fftw:+forward+)
        for inverse = (cl-fftw:%fft forward cl-fftw:+backward+) do
        (is (< (abs (- (row-major-aref forward 0)
                       (reduce #'+ (aops:flatten array))))
               1d-6))
        (map-into (aops:flatten array)
                  (lambda (x) (* x total))
                  (aops:flatten array))
        (is-true (arrays-almost-equal-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))

(test rfft
  (loop repeat 1000
        for rank  = (1+ (random 3))
        for dims  = (loop repeat rank collect (1+ (random 100)))
        for array = (make-array dims
                                :element-type 'double-float
                                :initial-contents (random-content dims :complexp nil))
        for total = (array-total-size array)
        for forward = (cl-fftw:%rfft array)
        for inverse = (cl-fftw:%irfft forward dims) do
        (is (< (abs (- (row-major-aref forward 0)
                       (reduce #'+ (aops:flatten array))))
               1d-6))
        (map-into (aops:flatten array)
                  (lambda (x) (* x total))
                  (aops:flatten array))
        (is-true (arrays-almost-equal-p
                  (aops:flatten array)
                  (aops:flatten inverse)))))
