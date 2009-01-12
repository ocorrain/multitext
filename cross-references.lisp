(in-package #:mess)

(publish-page
 "/cross-refs.html"
 (flet ((grab-checkboxes (id)
	  (mapcar (lambda (p)
		    (read-from-string (car p)))
		  (remove-if-not (lambda (elt)
				   (string-equal (cdr elt) id))
				 (tbnl:post-parameters)))))
   (lambda ()
     (with-tbnl-parameters (oid type searchterms)
       (when-bind (instance (find-instance oid))
	 (when-bind (to-link (grab-checkboxes "link"))
	   (dolist (l to-link) (link-by-oids oid l)))
	 (when-bind (to-unlink (grab-checkboxes "unlink"))
	   (dolist (l to-unlink) (unlink-by-oids oid l)))
	 (secure-page
	  (lambda () (get-priv (type-of instance)))
	  (lambda ()
	    (make-page "Cross references" :header (htmlo (:h1 "Cross references"))
		       :sidebar (htmlo (:h2 "Current cross-references")
				       (:ul (fmt "<li>~{~A~^</li><li>~}</li>" (mapcar (lambda (o)
											(format nil "~A (<i>~A</i>)"
												(display-link o)
												(make-presentable (symbol-name (class-name (class-of o))))))
										      (safe-sort (mapcar #'find-instance (xrefs instance))
												 #'string< :key (lambda (o)
														  (symbol-name (class-name (class-of o)))))))))
		       :content (htmlo (:h2 (str (display-short instance)))
				       (str (linked-class-selector :label "Create new cross-reference"
								   :destination "/cross-refs.html"
								   :method "post"
								   :additional-parms (list (list "oid" (oid instance)))))
				       (str (search-box (make-url "/cross-refs.html") "Find objects matching "
							(list (list "oid" (oid instance)))))
				       ((:form :action (make-url "/cross-refs.html") :method :post)
					(:input :type "submit" :value "Toggle xref")
					(:input :type "hidden" :name "oid" :value (oid instance))
					(when type (str (form-input "hidden" "type" type)))
					(when searchterms (str (form-input "hidden" "searchterms" searchterms)))
					(flet ((prep (o)
						 (let ((x (member (oid o) (xrefs instance))))
						   (list (form-input "checkbox" (oid o) (if x "unlink" "link"))
							 (format nil "<i>~A</i>"
								 (make-presentable
								  (symbol-name (class-name (class-of o))))) 
							 (format nil "~A~A"
								 (if x "<small><b>[LINKED]</b></small>" "")
								 (display-link o))))))
					  (cond (type (str (object-table (find-objects-of-type (intern (string-upcase type) 'mess))
									 :prep #'prep)))
						(searchterms (str (object-table (mapcar (lambda (r)
											  (find-instance (car r)))
											(search-results (format nil "~A" searchterms)))
										:prep #'prep)))
						(t (str (object-table (mapcar #'find-instance (xrefs instance)) :prep #'prep)))))))))))))))

