(in-package #:mess)

(publish-page "/linkobj.html"
	      (lambda ()
		(let* ((oid (parse-integer (tbnl:parameter "oid")))
		       (params (remove "oid" (tbnl:post-parameters) :test #'string-equal :key #'car))
		       (dels (mapcar #'car (remove "on" params :test #'string-equal :key #'cdr)))
		       (adds (mapcar #'car (remove "off" params :test #'string-equal :key #'cdr))))
		  (dolist (a adds)
		    (link-by-oids oid (parse-integer a)))
		  (dolist (d dels)
		    (unlink-by-oids oid (parse-integer d)))
		  (tbnl:redirect (add-get-params "/cross-refs.html" "oid" oid)))))

(publish-page "/sessions.html"
	      (secured (lambda () (when-bind (user (tbnl:session-value 'user))
			       (root user)))
		       (lambda ()
			 (make-page "Session information"
				    :header (htmlo (:h1 "Active sessions"))
				    :content (htmlo (if (tbnl:parameter "reap")
							(htm (tbnl:reset-sessions)
							     (:p "All sessions have been deleted.  You have been deleted.  Your powers are illusory.  Refresh the page and see for yourself."))
							(htm (:p ((:a :href (add-get-params (make-url "/sessions.html") "reap" "yes"))
								  "Reap all sessions"))))
						    (:table
						     (:tr (:th "User info")
							  (:th "Originating IP")
							  (:th "User agent"))
						     (tbnl:do-sessions (s)
						       (htm (:tr (:td (let ((user (tbnl:session-value 'user s)))
									(if user (htm (str (username user))
										      (:br) (:b (fmt "~{~A~^, ~}"
												     (mapcar (lambda (u)
													       (string-downcase (symbol-name u)))
													     (privs user))))
										      (:br) "Logged in "
										      (str (print-formatted-time (car (logins user)))))
									    (htm (:i "None")))))
								 (:td (str (tbnl:session-remote-addr s)))
								 (:td (str (tbnl:session-user-agent s))))))))))))


(publish-page "/edit.html"
	      (lambda ()
		(if-bind (oid (get-oid-param))
			 (progn
			   (log-msg 4 "EDIT -- got oid = ~A" oid)
			   (log-msg 4 "EDIT -- post parameters ~A" (tbnl:post-parameters))
			   (let ((instance (find-instance oid))
				 (autolink (get-tbnl-number "autolink"))
				 (link-this (get-tbnl-number "link-this")))
			     (log-msg 4 (format nil "EDIT -- instance ~A~%autolink ~A~%link-this ~A" instance autolink link-this))
			     (secure-page (lambda () (get-priv (type-of instance)))
					  (lambda ()
					    (log-msg 4 "Making editor")
					    (when link-this (link-by-oids (oid instance) link-this))
					    (make-page "Object editor"
						       :header (htmlo
								 (:h2 (fmt "Editing ~A" (display-link instance))))
						       :sidebar (htmlo (:h2 "Edit history")
								       (:ul
									(dolist (e (edit-history instance))
									  (htm (:li (str (print-formatted-time (second e)))))))
								       (when (get-priv (class-name (class-of instance)))
									 (str (edit-bar instance))))
						       :content (htmlo
								  (when (tbnl:post-parameters)
								    (->update oid (canonicalize-parameters (tbnl:post-parameters)))
								    (setf instance (find-instance oid))
								    (htm (:p "Object updated.")))
								  (str (make-button "Return to view mode" (make-view-url instance)))
								  (str (object->html-form instance :autolink autolink))
								  (str (display instance))))))))
			 (progn (log-msg 1 "EDIT -- could not get OID param")
				(make-page "Missing OID"
					   :content (htmlo (:p (str (tbnl:post-parameters)))))))))

      
(publish-page "/delete.html"
	      (lambda ()
		(when-bind (oid (get-oid-param))
		  (when-bind (instance (find-instance oid))
		    (log-msg 1 "DELETE -- deleting ~A" oid)
		    (secure-page (lambda () (get-priv (type-of instance)))
				 (lambda ()
				   (->delete oid)
				   (bounce)))))))

(publish-page "/createnew.html"
	      (lambda ()
		(let* ((symbol (intern (string-upcase (tbnl:parameter "type")) 'mess))
		       (instance (make symbol))
		       (oid (oid instance))
		       (redirect-url (add-get-params "/edit.html" "oid" oid))
		       (p (tbnl:parameter "autolink")))
		  (if p
		      (tbnl:redirect (add-get-params redirect-url "autolink" p))
		      (tbnl:redirect (add-get-params "/edit.html" "oid" oid))))))

(publish-page "/undelete.html"
	      (lambda ()
		(secure-page (lambda ()
			       (tbnl:session-value 'user))
			     (lambda ()
			       (with-std-page ("Undelete")
				 ((:form :action "/restore.html" :method "post")
				  (:table
				   (dolist (l *landfill*)
				     (htm (:tr
					   (:td
					    (:input :type "checkbox" :name (oid l) :value "yes"))
					   (:td
					    (fmt "~A" (display-short l))))))
				   (:tr (:td (:input :type "submit" :value "Undelete"))))))))))

(publish-page "/restore.html"
	      (lambda ()
		(let ((to-restore (mapcar #'parse-integer (mapcar #'car (tbnl:post-parameters)))))
		  (when to-restore
		    (dolist (r to-restore)
		      (let ((obj (find-instance r *landfill*)))
			(push obj *objects*)
			(setf *landfill* (delete obj *landfill*)))))
		  (tbnl:redirect "/view.html"))))

(publish-page "/robots.txt" (lambda () ""))

(defun list->table-cells (list)
  (format nil "<td valign='top'>~{~A~^</td><td valign='top'>~}</td>" list))

(defun form-input (type name value)
  (htmlo (:input :type type :name name :value value)))

(defun object-table
    (objs &key titles
     (prep (lambda (o)
	     (list (form-input "checkbox" (oid o) "unlink")
		   (format nil "<i>~A</i>" (make-presentable (symbol-name (class-name (class-of o)))))
		   (display-link o)))))
  (htmlo ((:table :width "100%")
	  (when titles
	    (htm (:tr (dolist (h titles) (htm (:th (str h)))))))
	  (dolist (o objs)
	    (htm (:tr (str (list->table-cells (funcall prep o)))))))))

(publish-page "/master-object-list.html"
	      (labels ((print-object-table (objs hidden)
			 (htmlo ((:form :action (make-url "/master-object-list.html") :method "post")
				 (:input :type "submit" :value "Delete")
				 (when hidden (htm (:input :type "hidden" :name (first hidden) :value (second hidden))))
				 (str (object-table objs :titles '("oid" "Type" "Name" "" "")
						    :prep (lambda (o)
							    (list (form-input "checkbox" (oid o) "unlink")
								  (format nil "<i>~A</i>" (make-presentable (symbol-name (class-name (class-of o)))))
								  (display-link o)
								  (format nil "<a href='~A'>[edit]</a>" (add-get-params (make-url "/edit.html") "oid" (oid o)))))))))))
		(lambda ()
		  (secure-page (lambda () (tbnl:session-value 'user)) 
			       (lambda ()
				 (make-page "Master object list"
					    :content (htmlo
						       (when-bind (deleted (remove-if-not #'numberp
											  (mapcar (lambda (p)
												    (read-from-string (car p)))
												  (tbnl:post-parameters)))) 
							 (htm (:ul
							       (dolist (d deleted)
								 (let ((obj  (find-instance d)))
								   (htm (:li (fmt "Deleted ~A" (display-short obj))))
								   (delete-object obj))))))
						       (str (linked-class-selector :label "View objects of type"
										   :destination "/master-object-list.html"
										   :method "get"))
						       (str (search-box (make-url "/master-object-list.html") "Find objects matching "))
						       (with-tbnl-parameters (type searchterms)
							 (cond  (type
								 (let ((typesym (intern (string-upcase type) (find-package 'mess))))
								   (if (member typesym *linked-classes*)
								       (htm (str (print-object-table (find-objects-of-type typesym)
												     (list "type" type)))))))
								(searchterms
								 (htm (str (print-object-table (mapcar (lambda (s) (find-instance (car s)))
												       (search-results searchterms))
											       (list "searchterms" searchterms)))))
								(t (htm (str "Please select a type from the drop-down list or enter a search string"))))))))))))

