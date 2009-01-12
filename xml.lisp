(in-package #:mess)

;;;; ------------------------------------------------------------------------
;;;; XML export routines

(defun xmlencode (tag cdata &optional (stream t) (indent 0))
  (flet ((xmlprint (tag cdata stream indent)
	   (if (= indent 0)
	       (format stream "<~A>~%~A</~A>~%" tag cdata tag)
	       (progn
		 (dotimes (i indent)
		   (princ " " stream))
		 (format stream "<~A>~A</~A>~%" tag cdata tag)))))
    (let ((tagname (string-downcase (symbol-name tag))))
      (if (consp cdata)
	  (dolist (c cdata)
	    (xmlprint tagname c stream indent))
	  (xmlprint tagname cdata stream indent)))))


(defmethod dump-object-to-xml ((object linked-class)) 
  (let* ((class (class-of object))
	(slots (mapcar #'slot-definition-name (class-slots class))))
    (with-output-to-string (s)
      (xmlencode (class-name class)
		 (with-output-to-string (s2)
		   (dolist (slot slots)
		     (if (slot-value-or-nil object slot)
			 (xmlencode slot (slot-value object slot) s2 5))))
		 s))))

(defun dump-objects-to-xml ()
  (log-msg 1 "Saving XML representation to disk")
  (with-open-file (xml *xml-export-file* :direction :output :if-exists :supersede)
    (dolist (obj *objects*)
      (format xml "~&~A~%" (dump-object-to-xml obj)))))



(defun length-of (something)
  (if (vectorp something)
      (length something)
      60))

