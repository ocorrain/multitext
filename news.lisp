(in-package #:mess)

(defmethod display ((obj news))
  (with-html-output-to-string (s)
    (:h2 (str (heading obj)))
    (:i (fmt "Posted at ~A" (print-formatted-time (posted obj))))
    (:p (str  (content obj)))))

(defmethod display-short ((obj news))
  (heading obj))

(define-page view-news "/view-news.html"
  (with-tbnl-parameters (since)
    (let* ((all-news (find-objects-of-type 'news))
	   (sincenumber (when since
			  (numberp (read-from-string since))))
	   (news-to-display (if sincenumber
				(remove-if-not (lambda (item)
						 (> (posted item)
						    (read-from-string since))) all-news)
				all-news)))
      (with-std-page ("News")
	((:div :class "contentwide")
	 (:h1 "News items")
	 (dolist (item (safe-sort news-to-display #'> :key #'posted))
	   (str (display item))))))))

