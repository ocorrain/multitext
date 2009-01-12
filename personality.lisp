(in-package #:mess)

(defmethod display ((obj personality))
  (with-html-output-to-string (s)
    (with-slots (name short-description dates short-biography) obj
      (htm (fmt "~A" name))
      (when dates
	(htm (fmt " (~A)" dates)))
      (if short-description
	  (htm (:br) (fmt "<i>~A</i>." short-description))
	  (htm (fmt ".")))
      (when short-biography
	(htm (:blockquote
	      (str (text->html short-biography))))))
    (str (display-data obj))))

(defmethod display-short ((obj personality))
  (with-html-output-to-string (s)
    (with-slots (name short-description dates short-biography) obj
      (htm ((:a :href (make-view-url obj))
	    (fmt "~A" name)))
      (if short-description
	  (htm (fmt ", <i>~A </i>" (string-downcase short-description)))))))

(defmethod display-heading ((obj personality))
  (with-html-output-to-string (s)
    (with-slots (name short-description dates)
	obj
      (if dates
	  (htm (:h1 (fmt "~A (~A)" name dates)))
	  (htm (:h1 (str name))))
      (if short-description
	  (htm (:h3 (str short-description)))))))

(defmethod display-nano ((obj personality))
  (format nil "~A" (name obj)))

