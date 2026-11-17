(in-package :cl-fftw/tests)

(def-suite one-dim/double :description "One dimensional transforms (double precision)")
(def-suite props/double :description "Generic FFT properties (double precision)")
(def-suite props/single :description "Generic FFT properties (single precision)")

(defun run-tests ()
  (let ((fiveam:*num-trials* 1000))
    (every #'identity
           (mapcar (lambda (suite)
                     (let ((status (run suite)))
                       (explain! status)
                       (results-status status)))
                   '(one-dim/double props/double props/single)))))

(defun gen-complex (f)
  (lambda ()
    (complex
     (funcall f)
     (funcall f))))

(defun gen-array (&key dimensions element-type elements)
  (lambda ()
    (let ((array (make-array (funcall dimensions)
                             :element-type element-type)))
      (loop for i below (array-total-size array)
            do (setf (row-major-aref array i)
                     (funcall elements)))
      array)))

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
(test fft/yaft/double
  (for-all ((array (gen-buffer :length        (gen-integer :min 2 :max 4000)
                               :element-type '(complex double-float)
                               :elements      (gen-complex
                                               (gen-float :bound  1d0
                                                          :type  'double-float)))))
  (let ((fft1 (yaft:fft array yaft:+forward+))
        (fft2 (cl-fftw/double:%fft array cl-fftw/core:+forward+)))
    (is-true (approx:array-approx-p fft1 fft2)))))

(test rfft/yaft/double
  (for-all* ((length (gen-integer :min 2 :max 4000))
             (array (gen-buffer :length        (gen-integer :min (* length 2)
                                                            :max (* length 2))
                                :element-type 'double-float
                                :elements      (gen-float :bound  1d0
                                                          :type  'double-float))))
    (let ((fft1 (yaft:rfft array))
          (fft2 (cl-fftw/double:%rfft array)))
      (is-true (approx:array-approx-p fft1 fft2)))))

(in-suite props/double)
(test fft/double
  (for-all ((array (gen-array
                    :dimensions    (gen-list :length   (gen-integer :min 1 :max 3)
                                             :elements (gen-integer :min 1 :max 100))
                    :element-type '(complex double-float)
                    :elements      (gen-complex (gen-float :bound 1d0 :type 'double-float)))))
    (let* ((total (array-total-size array))
           (forward (cl-fftw/double:%fft array   cl-fftw/core:+forward+))
           (inverse (cl-fftw/double:%fft forward cl-fftw/core:+backward+)))
      (is (approx:approxp (row-major-aref forward 0)
                          (vector-sum:sum (sb-ext:array-storage-vector array))))
      (map-into (sb-ext:array-storage-vector array)
                (lambda (x) (* x total))
                (sb-ext:array-storage-vector array))
      (is-true (approx:array-approx-p array inverse)))))

(test rfft/double
  (for-all ((array (gen-array
                    :dimensions    (gen-list :length   (gen-integer :min 1 :max 3)
                                             :elements (gen-integer :min 1 :max 100))
                    :element-type 'double-float
                    :elements      (gen-float :bound 1d0 :type 'double-float))))
    (let* ((total (array-total-size array))
           (forward (cl-fftw/double:%rfft  array))
           (inverse (cl-fftw/double:%irfft forward (array-dimensions array))))
      (is (approx:approxp (row-major-aref forward 0)
                          (vector-sum:sum (sb-ext:array-storage-vector array))))
      (map-into (sb-ext:array-storage-vector array)
                (lambda (x) (* x total))
                (sb-ext:array-storage-vector array))
      (is-true (approx:array-approx-p array inverse)))))

(in-suite props/single)
(test fft/single
  (for-all ((array (gen-array
                    :dimensions    (gen-list :length   (gen-integer :min 1 :max 3)
                                             :elements (gen-integer :min 1 :max 100))
                    :element-type '(complex single-float)
                    :elements      (gen-complex (gen-float :bound 1d0 :type 'single-float)))))
    (let* ((total (array-total-size array))
           (forward (cl-fftw/single:%fft array   cl-fftw/core:+forward+))
           (inverse (cl-fftw/single:%fft forward cl-fftw/core:+backward+)))
      (is (approx:approxp (row-major-aref forward 0)
                          (vector-sum:sum (sb-ext:array-storage-vector array))))
      (map-into (sb-ext:array-storage-vector array)
                (lambda (x) (* x total))
                (sb-ext:array-storage-vector array))
      (is-true (approx:array-approx-p array inverse)))))

(test rfft/single
  (for-all ((array (gen-array
                    :dimensions    (gen-list :length   (gen-integer :min 1 :max 3)
                                             :elements (gen-integer :min 1 :max 100))
                    :element-type 'single-float
                    :elements      (gen-float :bound 1d0 :type 'single-float))))
    (let* ((total (array-total-size array))
           (forward (cl-fftw/single:%rfft  array))
           (inverse (cl-fftw/single:%irfft forward (array-dimensions array))))
      (is (approx:approxp (row-major-aref forward 0)
                          (vector-sum:sum (sb-ext:array-storage-vector array))))
      (map-into (sb-ext:array-storage-vector array)
                (lambda (x) (* x total))
                (sb-ext:array-storage-vector array))
      (is-true (approx:array-approx-p array inverse)))))
