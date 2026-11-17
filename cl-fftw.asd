#-sbcl
(error "This wrapper is SBCL only")
(defsystem :cl-fftw/core
  :name :cl-fftw/core
  :version "0.3"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper for FFTW (general constants)"
  :licence "2-clause BSD"
  :pathname "core"
  :serial t
  :depends-on (:serapeum :cffi)
  :components ((:file "package")
               (:file "core")))

(defsystem :cl-fftw/double
  :name :cl-fftw/double
  :version "0.3"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper for FFTW (double precision)"
  :licence "2-clause BSD"
  :pathname "double"
  :serial t
  :components ((:file "package")
               (:file "fndb")
               (:file "wrapper")
               (:file "type-derivers"))
  :depends-on (:cl-fftw/core))

(defsystem :cl-fftw/single
  :name :cl-fftw/single
  :version "0.3"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper for FFTW (single precision)"
  :licence "2-clause BSD"
  :pathname "single"
  :serial t
  :components ((:file "package")
               (:file "fndb")
               (:file "wrapper")
               (:file "type-derivers"))
  :depends-on (:cl-fftw/core))

(defsystem :cl-fftw
  :name :cl-fftw
  :version "0.3"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper for FFTW"
  :licence "2-clause BSD"
  :depends-on (:cl-fftw/double :cl-fftw/single)
  :in-order-to ((test-op (load-op "cl-fftw/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-fftw/tests '#:run-tests)))

(defsystem :cl-fftw/tests
  :name :cl-fftw/tests
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:cl-fftw
               :yaft
               :fiveam
               :vector-sum
               :approx))

;; For qlot
(defsystem :cl-fftw/docs
  :name :cl-fftw/docs
  :depends-on (:cl-fftw :codex))
