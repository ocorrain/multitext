(in-package #:mess)

(defun backtraces ()
  (setf tbnl:*show-lisp-errors-p* t
	tbnl:*show-lisp-backtraces-p* t))

(defvar *url-names* nil)
(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defvar *mess-server* nil "Holds the server object")

(defparameter *image-directory* (concatenate 'string *dobdob-site* "db/images/"))

(defparameter *thumbs-directory* (concatenate 'string *image-directory* "thumbnails/"))

(defparameter *use-url-tags* 
  "Whether to use the mnemonic url names (experimental)")


(defun start-system ()
  (log-msg 1 "Starting system: ~A"
	   (print-formatted-time (get-universal-time)))
  (start-dobdob)
  (read-index)
  (when *use-url-tags*
    (log-msg 1 "Generating mnemonic URL tags")
    (setf *url-names*
	  (pairlis (mapcar #'oid *objects*)
		   (mapcar #'urlid *objects*))))
  (log-msg 1 "Starting web subsystem")
  (setf *mess-server*
	(hunchentoot:start-server :port 4242)) 
  (log-msg 1 "System ready.")
  'system-ready)


(defun stop-system ()
  (log-msg 1 "Stopping system")
  (log-msg 1 "Stopping web subsystem")
  (hunchentoot:stop-server *mess-server*)
  (stop-dobdob)
  (write-index)
  (dump-user-info)

  (log-msg 1 "System stopped ~A." 
	   (print-formatted-time (get-universal-time)))
  'system-stopped)

(defmethod print-object ((obj linked-class) s)
  (print-unreadable-object (obj s :type t :identity t )
    (format s "~A"  (display-nano obj))))



(define-class standard-display-mixin ())



(define-class subtopic (blob-data standard-display-mixin))

;; Multitext data structures

; toplevel
(define-visible-class field (linked-class)
  name start-date end-date)

; news items
(define-visible-class news (linked-class)
  heading content (posted (get-universal-time)))

; fields contain areas
(define-visible-class area (linked-class)
  name start-date end-date)

(define-visible-class chronology (linked-class standard-display-mixin)
  name tags)

(define-visible-class event (linked-class)
  day month year description to-day to-month to-year)

; areas contain topics
(define-visible-class topic (linked-class)
  name begin-year end-year)

; topics contain case-studies, personalities and concepts
(define-visible-class case-study (subtopic)
  name short-description)

(define-visible-class contributor (linked-class)
  full-name initials short-biography)

(define-visible-class personality (subtopic)
  name short-description dates short-biography)

(define-visible-class concept (subtopic)
  name description)

(define-visible-class perspective (subtopic)
  name author)

(define-visible-class gallery (linked-class)
  name description)


; Images
(define-class image (upload)
  filename)

(define-visible-class picture (image)
  "Short-caption should be exactly that, with a date if possible.  The long-caption can give as much detail as necessary.

Should it be possible, the source should be given as rigorously as feasible."
  name short-caption source long-caption
  (date 99999999 "This field is used for ordering purposes and will not be displayed.  Use YYYYMMDD format.  Pictures will be sorted lexicographically (e.g. 999 precedes 1234)"))

;; (define-visible-class picture (image)
;;   name short-caption source long-caption)

; Bibliographical categories
(define-visible-class text (blob-data standard-display-mixin)
  author title year publisher editor full-bibliographical-data)

(define-visible-class celt-text (linked-class standard-display-mixin)
  title author celt-number reference description)

(define-visible-class internet-reference (linked-class standard-display-mixin)
  author title year editor url)



;;;; ------------------------------------------------------------------------

(defmethod update-object :after ((obj linked-class) alist)
  ;; url id bookkeeping
  (declare (ignore alist))
  (unless (equal (urlid obj) (url-name obj))
    (setf (urlid obj) (get-url-tag-name obj))
    (push (cons (oid obj) (urlid obj)) *url-names*))
  
  (when (boundp 'tbnl:*session*)
    (push (list (username (tbnl:session-value 'user))
	      (get-universal-time)) (edit-history obj))) 
  
  (setf *url-names* (remove-duplicates *url-names* :test #'equal)
	(edit-history obj) (remove-duplicates (edit-history obj) :test #'equal))

  (deindex-object (oid obj))
  (index-object obj)
;;   (dump-objects)
  )

(defmethod get-url-tag-name ((obj linked-class))
  (let* ((cand (url-name obj))
	 (found (rassoc cand *url-names* :test #'string-equal)))
    (if (or (not found) (equal (car found) (oid obj)))
	cand
	(format nil "~A~A" cand (get-universal-time)))))


(defmethod url-name ((obj linked-class))
  (remove-if-not (lambda (c) (or (alphanumericp c)
			    (char= c #\_)
			    (char= c #\-)))
		 (cl-ppcre:regex-replace-all "\\s+"
					     (display-nano obj) "_")))

(defmethod url-name ((obj contributor))
  (if (initials obj)
      (format nil "Contributor-~A-~A" (initials obj) (oid obj))
      (format nil "Contributor-~A" (oid obj))))

(defun disambiguate-name (string)
  (if (not (rassoc string *url-names* :test #'string-equal))
      string
      (format nil "~A~A" string (get-universal-time))))

(defmethod update-object :around ((obj blob-data) alist)
  (let ((file-info (cdr (assoc "filename" alist :test #'string-equal)))
	(oid (oid obj)))
    (log-msg 6 "update-object -- file info is ~S" file-info)
    (when file-info
      (when (probe-file (first file-info))
	(let ((temp-filename (first file-info))
	    (content-type (third file-info)))
	(html-clean-to-file temp-filename (make-db-path oid))
	(setf (headings obj) (rewrite-headings (oid obj)))
	(setf (file-content-type obj) content-type))))
    (call-next-method obj (remove-if #'upload-specific alist))))

(defun upload-specific (e)
  (or (string-equal (car e) "file-content-type")
      (string-equal (car e) "filename")))

(defun make-db-path (oid)
  (format nil "~A~A" (blob-directory) oid))


(defun delete-if-exists (filename)
  (when (probe-file filename)
    (delete-file filename)))

(defmethod delete-object :before ((obj blob-data))
  (delete-if-exists (make-db-path (oid obj))))

(defmethod delete-object :before ((obj image))
  (dolist (dir (list *image-directory* *thumbs-directory* (blob-directory)))
    (delete-if-exists (format nil "~A~A" dir (filename obj)))))

(defun get-objects-of-type (symbol)
  (if (eql symbol 'all)
      *objects*
      (remove-if-not #'(lambda (obj)
			 (eql (type-of obj) symbol)) *objects*)))

(defmethod object-sort ((obj1 personality) (obj2 personality))
  (flet ((get-surname (obj)
	   (first (last (cl-ppcre:split "\\s+" (name obj))))))
    (string-lessp (get-surname obj1) (get-surname obj2))))

(defmethod object-sort ((obj1 topic) (obj2 topic))
 (string-lessp (begin-year obj1) (begin-year obj2)))


(defmethod object-sort ((obj1 concept) (obj2 concept))
  (string-lessp (name obj1) (name obj2)))

;;;; -----------------------------------------------------------------------
;;;; Transaction logging functions


