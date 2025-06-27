(defun do-all()
  (ql:quickload :cl-fftw/tests)
  (uiop:quit
   (if (uiop:call-function "cl-fftw/tests:run-tests")
       0 1)))

(do-all)
