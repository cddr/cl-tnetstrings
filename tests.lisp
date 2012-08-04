
(defpackage :tnetstrings-test
  (:use :cl :tnetstrings :cl-test-more))

(in-package :tnetstrings-test)

(deftest basic-tests
    (let ((int 42)
          (float 42.0)
          (str "forty two")
          (list `(1 2 3))
          (dict (dict "a" 1 "b" 2))
          (true t)
          (false nil))
      (is int (parse (dump int)) "integer")
      (is float (parse (dump float)) "float")
      (is str (parse (dump str)) "string")
      (is list (parse (dump list)) "list")
      (is dict (parse (dump dict)) "dict" :test #'equalp)
      (is true t "true")
      (is false nil "false")))