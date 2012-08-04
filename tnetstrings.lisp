
(in-package :tnetstrings)

(defun dict (&rest rest)
  "Syntactic sugar for creating hash-tables

Returns a hash-table in which every odd element of `&rest' is a key
and every even element is the corresponding value"
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on rest by #'cddr
         do (setf (gethash k ht) v)
         finally (return ht))))

(defun dump (data)
  "Serialize `data' as a typed netstring.

`data' can be one of the following types
integer
float
string
hash-table (aka dict)
list
boolean
"
  (typecase data
    (integer (tnet-fmt data #\#))
    (float (tnet-fmt data #\^))
    (string (tnet-fmt data #\,))
    (hash-table (dump-dict data))
    (list (dump-list data))
    (boolean (tnet-fmt (if data
                           "true"
                           "false") #\!))
    (otherwise (error "Can't serialize stuff that's ~a" (type-of data)))))

(defun dump-dict (data)
  (let ((result))
    (maphash (lambda (k v)
               (push (dump (format nil "~a" k)) result)
               (push (dump v) result))
             data)
    (let ((payload (tnet-join "" (reverse result))))
      (tnet-fmt payload #\}))))

(defun dump-list (data)
  (let* ((result (mapcar 'dump data))
         (payload (tnet-join "" result)))
    (tnet-fmt payload #\])))

(defun parse (data)
  "De-Serialize the typed netstring specified by `data'"
  (multiple-value-bind (payload payload-type remain)
      (parse-payload data)
    (case payload-type
      (#\# (values (parse-integer payload) remain))
      (#\} (values (parse-dict payload) remain))
      (#\] (values (parse-list payload) remain))
      (#\! (values (string= payload "true") remain))
      (#\^ (values (read-from-string payload) remain))
      (#\~ (values nil remain))
      (#\, (values payload remain))
      (otherwise (error "Invalid payload type: ~a" payload-type)))))

(defun parse-payload (data)
  (assert data () "Invalid data to parse. it's empty.")
  (multiple-value-bind (len extra)
      (tnet-split #\: data)
    (setf len (parse-integer len))
    (let ((payload (subseq extra 0 len))
          (extra (subseq extra len)))
      (let ((payload-type (aref extra 0))
            (remain (subseq extra 1)))

        (values payload
                payload-type
                (if (string= remain "") nil remain))))))

(defun parse-list (data)
  (if (= (length data) 0)
      nil
      (loop with result = () and val = nil and extra = data
         until (not extra)
         do (multiple-value-setq (val extra)
              (parse extra))
           (push val result)     
         finally (return (nreverse result)))))

(defun parse-dict (data)
  (let ((ht (dict)))
    (if (= (length data) 0)
        ht
        (loop with k = nil and v = nil and extra = data
           until (not extra)
           do (multiple-value-setq (k v extra)
                (parse-pair extra))
             (setf (gethash k ht) v)
           finally (return ht)))))

(defun parse-pair (data)
  (multiple-value-bind (key extra)
      (parse data)
    (assert extra () "Unbalanced dictionary store.")
    (multiple-value-bind (val extra)
        (parse extra)
      (values key val extra))))      

(defun tnet-split (sep str)
  (let* ((pos (position sep str))
         (remain (when pos
                   (subseq str (1+ pos)))))
    (values (subseq str 0 pos)
            remain)))

(defun tnet-fmt (data marker)
  (let* ((out (format nil "~a" data))
         (len (length out)))
    (format nil "~a:~a~a"
            len out marker)))

(defun tnet-join (sep items)
  (with-output-to-string (s)
    (loop for (item . rest) on items
         do (princ item s)
         if rest do
         (princ sep s))))         


