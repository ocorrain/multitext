(in-package #:mess)

;; really trivial implementation of transaction logging, writes logged functions to
;; a transaction log file. Will not work with lambda-keywords, or recursive functions.



(defvar *transaction-logging* t "Should we log transactions?")

(defun write-to-transaction-log (stuff)
  (let ((*print-readably* t))
    (when *transaction-logging*
      (with-open-file (transaction-log *transaction-log*
				       :direction :output :if-exists :append :if-does-not-exist :create)
	(write stuff :stream transaction-log)))))

(defun read-transaction-log ()
  (let ((*transaction-logging* nil))
    (with-open-file (txn *transaction-log*)
      (flet ((aread (stream)
	       (read stream nil 'DONE)))
	(do ((sexp (aread txn) (aread txn))
	     (counter 0 (1+ counter)))
	    ((eq sexp 'done) (log-msg 2 "Replayed ~A transactions"
				      counter))
	  (log-msg 2 "    ~A -> ~A" (car sexp) (cdr sexp))
	  (apply (car sexp) (cdr sexp)))))
    (delete-file *transaction-log*)))

(defmacro define-logged-function (name args &body body)
  `(defun ,name ,args
     (when *transaction-logging* (write-to-transaction-log (list ',name ,@args))) 
     ,@body))

