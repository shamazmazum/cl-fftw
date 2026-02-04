(defun do-all()
  (handler-case
      (asdf:load-system :cl-fftw/tests)
    (asdf:compile-file-error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "cl-fftw/tests:run-tests")
       0 1)))

(do-all)
