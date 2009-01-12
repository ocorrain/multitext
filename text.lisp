(in-package #:mess)

(defmethod navigation-bar ((obj text))
  (with-html-output-to-string (s)
    ((:div :class "navbar")
     ((:a :href (make-url "/") :class "navbar")
      "HOME")
     " >> "
     (str (bounce-url 1)))))


(defmethod render ((obj text))
  (with-html-output-to-string (s)
    (str (navigation-bar obj)) 
    (when (get-priv (class-name (class-of obj)))
      (htm ((:div :class "contentwide")
	    (str (edit-bar obj)))))
    ((:div :class "contentwide")

     ((:a :class "printerfriendly" :href (format nil
						 "/printable/~A" (oid obj)))
       "PRINTER-FRIENDLY VERSION")
     
     (str (display obj)))))

(defmethod display ((obj blob-data))
  (with-html-output-to-string (s)
    (:h2 (str (display-short obj)))
    (str (display-data obj))))

(defmethod display-data ((obj blob-data))
  (let ((content-type (file-content-type obj))
	(text (string<-text (oid obj))))
    (when text
      (with-html-output-to-string (s)
      (cond ((string-equal content-type "text/plain") (htm (:pre (str text))))
	    (t (str text)))))))

(defmethod display-short ((obj text))
  (with-html-output-to-string (s)
    (with-slots (oid author title year publisher editor) obj
      (when author
	(htm (fmt "~A, " author)))
      (htm ((:a :href (make-view-url obj))
	    (:i (fmt "~A" title))))
       (cond ((and year publisher) (htm (fmt " (~A, ~A)." year publisher)))
	     (year (htm (fmt " (~A)." year)))
	     (publisher (htm (fmt " (~A)." publisher)))
	     (t (htm (fmt "."))))
       (when editor
	 (htm (fmt " ~A (ed)." editor))))))

(defmethod display-nano ((obj text))
  (with-slots (author title year)
      obj
    (with-output-to-string (s)
      (when author
	(format s "~A, " author))
      (format s "\"~A\"" title)
      (when year
	(format s " (~A)" year)))))

(defmethod find-topic ((obj text))
  (when-bind (subtopic (find-if (lambda (obj)
				  (member (find-class 'subtopic)
					  (class-precedence-list
					   (class-of obj))))
				(mapcar #'find-instance (xrefs obj))))
    (find-topic subtopic)))
