;; Stolen from MAGICL

(defclass c->so (asdf:source-file)
  ((link-with :initarg  :link-with
              :initform (error "LINK-WITH is not specified")
              :reader   link-with))
  (:default-initargs
   :type "c"))

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))

(defmethod output-files ((operation compile-op) (component c->so))
  (values (list (asdf:apply-output-translations
                 (make-pathname :name "libfftwrapd"
                                :type (dynamic-library-extension)
                                :defaults (component-pathname component))))
          t))

(defmethod perform ((operation load-op) (component c->so))
  t)

(defmethod perform ((operation compile-op) (component c->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((c-file (component-pathname component))
           (shared-object (first (output-files operation component))))
      (ensure-directories-exist shared-object)
      (uiop:run-program
       (list "cc" "-fPIC" "-shared"
             #+freebsd "-I/usr/local/include"
             #+freebsd "-L/usr/local/lib"
             "-o" (nn shared-object) (nn c-file)
             (format nil "-l~a" (link-with component)))))))

(defsystem :cl-fftw
  :name :cl-fftw
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "A wrapper for FFTW"
  :licence "2-clause BSD"
  :pathname "src"
  :serial t
  :components ((:file  "package")
               (:c->so "libfftwrapd" :link-with "fftw3")
               (:file  "wrapper"))
  :depends-on (:cffi :serapeum)
  :in-order-to ((test-op (load-op "cl-fftw/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-fftw/tests '#:run-tests)))

(defsystem :cl-fftw/tests
    :name :cl-fftw/tests
    :pathname "tests"
    :components ((:file "package")
                 (:file "tests" :depends-on ("package")))
    :depends-on (:cl-fftw :yaft :array-operations :fiveam))
