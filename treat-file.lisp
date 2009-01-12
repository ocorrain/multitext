(in-package #:mess)

(defparameter *permitted-tags* (list "i" "/i"
				     "b" "/b"
				     "p" "/p"
				     "em" "/em"
				     "table" "/table"
				     "th" "/th"
				     "caption" "/caption"
				     "tr" "/tr"
				     "td" "/td"
				     "h1" "/h1"
				     "h2" "/h2"
				     "h3" "/h3"
				     "h4" "/h4"
				     "blockquote" "/blockquote"
				     "ol" "/ol"
				     "li" "/li"
				     "ul" "/ul"
				     "pre" "/pre"
				     "hr" "br"
				     "colgroup" "/colgroup"))

(defparameter *parameterized-tags* (list "a" "/a" "tr" "/tr" "td" "/td"
					 "th" "/th" "col" "/col" "table" "/table"))

(defun get-headings (stream)
  (with-open-stream (f stream)
    (let ((out nil))
      (do ((l (read-line f nil 'done) (read-line f nil 'done)))
	  ((eq l 'done) (reverse out))
	(when (stringp l)
	  (cl-ppcre:register-groups-bind (level cdata)
	      ("<[hH]([1-6])>(.*)<\/[hH][1-6]>" l )
	    (when (and level cdata (not (equal cdata "")))
	      (setf out (acons (parse-integer level) cdata out)))))))))

(defun html->text (string)
  (when string
    (with-output-to-string (out)
    (with-input-from-string (in string)
      (labels ((worker (in-tag in-entity)
		 (when-bind (c (read-char in nil nil))
		   (cond (in-tag (if (char-equal c #\>)
				      (worker nil nil)
				      (worker t nil)))
			 (in-entity (if (or (char-equal c #\Space)
					    (char-equal c #\;))
					(worker nil nil)
					(progn (format out " ")
					       (worker nil t)) ))
			 (t (cond ((char-equal c #\<) (worker t nil))
				  ((char-equal c #\&) (worker nil t))
				  (t (format out "~A" c)
				     (worker nil nil))))))))
	(worker nil nil))))))

(defun tagger (istream ostream)
  (do ((c (read-char istream nil nil) (read-char istream nil nil)))
      ((eq c nil) 'done)
    (if (char= c #\<)
	(let ((tag? (handle-tag istream)))
	  (when tag?
	    (format ostream "~A" tag?)))
	(write-char c ostream))))

(defun tokenizer (istream ostream &optional state token)
  (let ((c (read-char istream nil nil)))
    (if (null c)
	nil
	(case state
	  (in-tag (if (char= c #\>)
		      (cons (list 'tag (concatenate 'string token)) (tokenizer istream ostream))
		      (tokenizer istream ostream 'in-tag (append token (list c)))))
	  (in-entity (if (char= c #\;)
			 (cons (list 'entity (concatenate 'string token)) (tokenizer istream ostream))
			 (tokenizer istream ostream 'in-entity (append token (list c)))))
	  (otherwise (cond ((char= c #\<)
			    (cons token (tokenizer istream ostream 'in-tag)))
			   ((char= c #\&)
			    (cons (list 'cdata (concatenate 'string token)) (tokenizer istream ostream 'in-entity)))
			   (t (tokenizer istream ostream nil (append token (list c))))))))))


(defun handle-tag (istream)
  "Grab the tag from the input buffer, and return it if on the
   list of permitted tags. Otherwise return nil"
  (labels ((worker (istream tag-text)
	     (let ((c (read-char istream)))
	       (when c
		 (if (char= c #\>)
		     (decode-tag tag-text)
		     (worker istream 
			     (concatenate 'string
					  tag-text
					  (string c))))))))
    (worker istream "")))

(defun decode-tag (tag-text)
  "If the tag is permitted, return it.write it to
   ostream without its parameters, at least for the moment.
   Otherwise do nothing"
  (let* ((dectag (cl-ppcre:split "\\s+" tag-text :limit 2))
	 (tag-name (car dectag))
	 (tag-params (cadr dectag)))
    (cond ((parameterized-tag-p tag-name)
	   (if tag-params
	       (format nil "<~A ~A>" (string-downcase tag-name) tag-params)
	       (format nil "<~A>" (string-downcase tag-name))))
	  ((permitted-tag-p tag-name)
	   (format nil "<~A>" (string-downcase  tag-name)))
	  (t nil))))

(defun find-subclasses (symbol &optional (objects *objects*))
  (remove-if-not (lambda (o)
		   (member symbol (mapcar #'class-name
					  (class-precedence-list
					   (class-of o))) ))
		 objects))


;(defun one-sided-tag-p (tag-type)
;  (find tag-type *one-sided-tags* :test #'string-equal))

(defun permitted-tag-p (tag-type)
  (find tag-type *permitted-tags* :test #'string-equal))

(defun parameterized-tag-p (tag-type)
  (find tag-type *parameterized-tags* :test #'string-equal))

(defun html-clean (filename)
  "Reads HTML from filename and returns a string
   containing the cleaned HTML"
  (let ((os (make-string-output-stream)))
    (with-open-file (f filename)
      (tagger f os))
    (get-output-stream-string os)))

(defun html-clean-to-file (oldfile newfile)
  (with-open-file (in oldfile)
    (with-open-file (out newfile :direction :output :if-exists :supersede)
      (tagger in out))))

(defun string<-text (oid)
  (when (probe-file (make-db-path oid))
    (let ((os (make-string-output-stream)))
    (with-open-file (f (make-db-path oid))
      (do ((c (read-char f nil nil) (read-char f nil nil)))
	  ((eq c nil) 'done)
	(princ c os)))
    (get-output-stream-string os))))

(defun rewrite-headings (oid)
  (flet ((wikify (string)
	   (remove-if-not #'alphanumericp string)))
    (when-bind (text (string<-text oid))
      (let ((headings (remove-if-not (lambda (h)
				       (= (car h) 2))
				     (with-input-from-string (s text)
				       (get-headings s)))))
	(dolist (h headings)
	  (setf text
		(cl-ppcre:regex-replace-all
		 (format nil "(<a name=\".*\">)*<[hH]~A>~A<\\/[hH]~A>(</a>)*"
			 (car h) (cdr h) (car h))
		 text (format nil "<a name=\"~A\"><h~A>~A</h~A></a>"
			      (wikify (cdr h)) (car h) (cdr h) (car h)))))
	(with-open-file (f (make-db-path oid)
			   :direction :output :if-exists :supersede)
	  (format f "~A" text))
	(values (mapcar (lambda (h)
			  (list (cdr h) (wikify (cdr h)))) headings)
		text)))))

(defmethod clean-text ((obj blob-data))
  (let ((dbfile (make-db-path (oid obj))))
    (when (probe-file dbfile)
      (let ((cleaned (html-clean (make-db-path (oid obj)))))
	(with-open-file (f (make-db-path (oid obj)) :direction :output :if-exists :supersede)
	  (format f "~A" cleaned))))))

