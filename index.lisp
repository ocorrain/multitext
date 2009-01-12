(in-package #:mess)

(defvar *db-index* (make-hash-table :test #'equal))
(defvar *excluded-words* (list "it" "she" "he" "with" "from" "for" "by" "and" "of" "a" "an" "the" "there"))

(defun clear-index ()
  (clrhash *db-index*))

(defun get-index (item &optional (index *db-index*))
  (safe-sort (gethash (string-downcase item) index) #'> :key #'cdr))

(defun put-index (item value freq &optional (index *db-index*))
  (unless (excluded? item)
    (pushnew (cons value freq) (gethash item index nil))))

(defun index->alist (&key (index *db-index*) sort)
  (let (list)
    (maphash (lambda (k v) (push (list k v) list)) index)
    (if sort
	(sort list sort :key #'car)
	list)))

(defun write-index (&optional (filename *index-file*))
  (log-msg 1 "Writing index to ~A" filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (write *excluded-words* :stream f)
    (write (index->alist) :stream f))
  'written)

(defun read-index (&optional (filename *index-file*))
  (if (probe-file filename)
      (progn (log-msg 1 "Reading index from ~A" filename)
	     (with-open-file (f filename)
	       (setf *excluded-words* (read f))
	       (dolist (elt (read f))
		 (setf (gethash (car elt) *db-index*) (cadr elt)))))
      (progn (log-msg 1 "Index file not found. Creating fresh index.")
	     (index-objects *objects*)
	     (write-index filename)))
  (length (keys)))

(defun print-index (alist &optional (stream t))
  (format stream "~&~{~A~^~%~}" alist))

(defun ban-word (word &optional (index *db-index*))
  (remhash word index)
  (pushnew word *excluded-words* :test #'equal))

(defun excluded? (word)
  (member word *excluded-words* :test #'equal))

(defun index-string (string)
  (remove-if (lambda (word) (or (equal word "") (excluded? word)))
	     (mapcar (lambda (s) (remove-if-not #'alphanumericp s))
		     (cl-ppcre:split "\\s+" string))))

(defmethod index-object ((obj linked-class))
  (let ((counts (make-hash-table :test (function equal)))
	(words (reduce #'nconc (mapcar #'index-string (index-items obj)))))
    (dolist (item words) (incf (gethash item counts 0)))
    (maphash (lambda (k v) (put-index (string-downcase k) (oid obj) v)) counts)))

(defun deindex-object (oid &optional (index *db-index*))
  (maphash (lambda (k v) (setf (gethash k index) (remove oid v :key #'car))) index))

(defmethod delete-object :before ((obj linked-class))
  (deindex-object (oid obj)))

(defmethod index-items ((obj linked-class))
  nil)

(defmethod index-items ((obj text))
  (with-slots (author title year publisher editor full-bibliographical-data) obj
      (list author title year publisher editor full-bibliographical-data (html->text (display-data obj)))))

(defmethod index-items ((obj picture))
  (with-slots (short-caption long-caption source) obj
    (list short-caption long-caption source)))

(defmethod index-items ((obj personality))
  (with-slots (name short-description dates) obj
    (list name short-description dates (html->text (display-data obj)))))

(defmethod index-items ((obj concept))
  (list (name obj) (html->text (display-data obj))))

(defmethod index-items ((obj case-study))
  (list (name obj) (html->text (display-data obj))))

(defmethod index-items ((obj perspective))
  (list (name obj) (author obj) (html->text (display-data obj))))

(defun index-objects (list)
  (dolist (l list)
    (index-object l)))

(defun keys (&optional (index *db-index*))
  (let (list)
    (maphash (lambda (k v) (push k list)) index)
    list))

(defun search-index (search-string)
  (mapcar #'get-index (index-string search-string)))

(defun search-results (search-string)
  (let ((count (make-hash-table)))
    (dolist (item (reduce #'nconc (search-index search-string)))
      (let ((cur (gethash (car item) count)))
	(if cur
	    (setf (gethash (car item) count) (cons (1+ (car cur))
						   (+ (cdr cur) (cdr item))))
	    (setf (gethash (car item) count) (cons 1 (cdr item))))))
    (let ((shuffled (make-hash-table)))
      (maphash (lambda (k v) (pushnew (cons k (cdr v)) (gethash (car v) shuffled nil))) count)
      (let ((results (make-array (hash-table-count count) :fill-pointer 0)))
	(maphash (lambda (k v) (vector-push (cons k (sort v #'> :key #'cdr)) results)) shuffled)
	(reduce #'append (mapcar #'cdr (coerce (sort results #'> :key #'car) 'list)))))))

(defun search-example (search-string)
  (dolist (hit (search-results search-string))
    (format t "~&~A~%" (display-nano (find-instance (car hit))))))

(publish-page "/search.html"
	      (lambda ()
		(let ((search-string (tbnl:parameter "searchterms")))
		  (if (and search-string (not (zerop (length search-string))))
		      (make-page "Search results"
				 :header (htmlo (:h2 (fmt "Search results for ~A" search-string)))
				 :sidebar ""
				 :content (web-search-results search-string))
		      (bounce)))))

(defun web-search-results (search-string)
  (htmlo ((:div :id "searchresults")
	  (:ul
	   (dolist (hit (search-results search-string))
	     (htm (:li (str (search-result (car hit))))))))))

(defun search-result (hit)
  (if-bind (inst (find-instance hit))
	   (or (search-text inst)
	       (update-search-text inst))
	   ""))

(defun update-search-text (obj)
  (setf (search-text obj) (display-search-hit obj)))

(defmethod display-search-hit ((hit linked-class))
  (htmlo  (fmt "<i>~A</i> " (make-presentable
			       (symbol-name
				(class-name
				 (class-of hit)))))
	  (str (display-link hit))))

(defparameter *gobbet-size* 300 "The amount of text, in
characters, to display in the search results")

(defmethod display-search-hit ((hit picture))
  (htmlo (:i "Picture ")
	 (str (display-link hit)) (:br)
	 (when-bind (lc (long-caption hit))
	   (str (teaser *gobbet-size* lc)))))

(defmethod display-search-hit ((hit blob-data))
  (htmlo (:i (fmt "~A " (make-presentable (symbol-name (class-from-obj hit)))))
	 (str (display-link hit)) (:br)
	 (when-bind (lc (cl-ppcre:regex-replace-all "\\s+" (html->text (display-data hit))
						    " "))
	   (str (teaser *gobbet-size* lc)))))

(defun teaser (length string)
  (if string
      (let ((gobbet (subseq string 0 (min length (length string)))))
	(if (> (length string) length)
	    (format nil "~A [...]" gobbet)
	    gobbet))
      ""))

