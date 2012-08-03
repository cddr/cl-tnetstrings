
(in-package :tnetstrings)

;; Helpers

(defun join (items)
  (with-output-to-string (s)
    (loop for item in items
       do (princ item s))))

(defun dict (&rest alist)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on alist by #'cddr
         do (setf (gethash (string k) ht) v))
    ht))

(defun tfmt (fmt data)
  (let* ((out (format nil "~a" data))
         (len (length out)))
    (format nil fmt len out)))

;; Implementation

(defun dump (data)
  (etypecase data
    (integer (tfmt "~a:~a#" data))
    (float (tfmt "~a:~a^" data))
    (string (tfmt "~a:~a," data))
    (list (dump-list data))
    (hash-table (dump-dict data))
    (boolean (tfmt "~a:~a!" (if data "true" "false")))))

(defun parse (data)
  (multiple-value-bind (payload payload-type remainder)
      (parse-payload data)
    (case payload-type
      (#\# (values (parse-integer payload) remainder))
      (#\} (values (parse-dict payload) remainder))
      (#\] (values (parse-list payload) remainder))
      (#\! (values (string= payload "true") remainder))
      (#\^ (values (read-from-string payload) remainder))
      (#\~ (progn
             (assert (= (length payload) 0) () "Payload must be 0 length for null.")
             (values nil remainder)))
      (#\, (values payload remainder)))))


(defun parse-payload (data)
  (multiple-value-bind (len extra)
      (let ((pos (position #\: data)))
        (values (parse-integer (subseq data 0 pos))
                (subseq data (+ 1 pos))))

    (multiple-value-bind (payload extra)
        (values (subseq extra 0 len)
                (subseq extra len))

      (multiple-value-bind (payload-type remain)
          (values (aref extra 0)
                  (subseq extra 1))

        (values payload payload-type 
                (if (string= "" remain) nil remain))))))
      
(defun parse-list (data)
  (if (= (length data) 0)
      nil
      (loop with result = nil
         for (value extra) = (multiple-value-list (parse data)) 
         then (multiple-value-list (parse extra))
         until (not extra)
         do (push value result)
         finally (return (progn
                           (push value result)
                           (reverse result))))))

(defun parse-pair (data)
  (multiple-value-bind (key extra)
      (parse data)
    (multiple-value-bind (val extra)
        (parse extra)
      (values key val extra))))

(defun parse-dict (data)
  (if (= (length data) 0)
      (make-hash-table :test 'equal)
      (loop with result = (make-hash-table :test 'equal)
         for (key value extra) = (multiple-value-list (parse-pair data))
         then (multiple-value-list (parse-pair extra))
         until (not extra)
         do (setf (gethash key result) value)
         finally (return (progn
                           (setf (gethash key result) value)
                           result)))))

(defun dump-dict (dict)
  (let ((result))
    (maphash (lambda (k v)
               (push (dump (format nil "~a" k)) result)
               (push (dump v) result))
             dict)
    (let ((payload (join (reverse result))))
      (tfmt "~a:~a}" payload))))

(defun dump-list (list)
  (let ((result))
    (loop for item in list
         do (push (dump item) result))
    (let ((payload (join (reverse result))))
      (tfmt "~a:~a]" payload))))

