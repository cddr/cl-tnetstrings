

(defsystem :cl-tnetstrings-test
  :serial t
  :depends-on (cl-test-more)
  :components ((:file "tests")))

(defsystem :cl-tnetstrings
  :description "Common Lisp implementation of tnetstrings http://tnetstrings.org/"
  :serial t
  :components ((:file "package")
               (:file "tnetstrings"))
  :in-order-to ((test-op (load-op cl-tnetstrings-test)))
  :perform (test-op :after (op c)
             (funcall
              (intern (format nil "~a" 'run-test-package)
                      (find-package :cl-test-more))
              :tnetstrings-test)))

