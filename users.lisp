(in-package #:mess)

(define-page user-page "/userdetails.html"
  "Moby function to handle users and privileges"
  (let ((current-user (tbnl:session-value 'user)))
    (secure-page (lambda ()
		   (and current-user (root current-user)))
		 (lambda ()
		   (with-tbnl-parameters (username)
		     (with-std-page ("User information")
		       ((:div :id "navBeta")
			(str (last-links)))
		       ((:div :class "contentwide")
			(if username
			    (str (operate-on-user username));; do number 1
			    (str (user-selector))))))))))

(defun operate-on-user (username)
  (let ((udetails (find username *users* :test #'string-equal :key #'username))
	(current-user (tbnl:session-value 'user)))
    (with-tbnl-parameters (oldpass newpass1 newpass2 changeprivs)
      (with-html-output-to-string (s)
	(if udetails
	    (htm (:h2 (fmt "Details for ~A" username))
		 (:p (fmt "Logins: ~{~A~^; ~}" (mapcar (lambda (ut)
							 (print-formatted-time ut))
						     (logins udetails))))
		 (if (root current-user)
		     (if changeprivs
			 (str (change-privs udetails))
			 (str (priv-form udetails)))
		     (htm (:p (fmt "Can edit ~{~A~^, ~}." (mapcar (lambda (o)
								    (make-presentable (symbol-name o)))
								  (privs udetails))))))
		 (if (and oldpass newpass1 newpass2)
		     (str (change-password udetails oldpass newpass1 newpass2))
		     (str (password-form udetails))))
	    (str "No such user!"))))))

(defun user-selector ()
  (with-html-output-to-string (s)
    (with-tbnl-parameters (addnewuser addnew)
      (cond (addnew (str (new-user-form)))
	    (addnewuser (str (add-new-user)))
	    (t (htm (:b (fmt "Edit user details for ~{~A~^, ~}."
			     (mapcar (lambda (u)
				       (with-html-output-to-string (s)
					 ((:a :href
					      (add-get-params
					       (make-url "/userdetails.html")
					       "username" (username u)))
					  (str (username u))))) *users*)))
		    (:p ((:a :href (add-get-params
				    (make-url "/userdetails.html")
				    "addnew" "yes"))
			 "Add a new user"))))))))


(defun get-priv (priv)
  (let ((user (tbnl:session-value 'user)))
    (when user
      (has-priv user priv))))


(defun redirect (oid)
  (if oid
      (tbnl:redirect (add-get-params "/viewobj.html" "oid" oid))
      (tbnl:redirect "/view.html")))

(define-page authenticate-page "/auth.html"
  (let ((username (tbnl:parameter "username"))
	(password (tbnl:parameter "password")))
    (let ((user (authenticate-user username password)))
      (if user
	  (progn (tbnl:start-session)
		 (push (get-universal-time) (logins user))
		 (setf (tbnl:session-value 'user) user)
		 (log-msg 1 "~A logged in." (username user))
		 (bounce))
	  (bounce)))))

(define-page logout-page "/logout.html"
  (log-msg 1 "~A logged out." (username (tbnl:session-value 'user)))
  (setf (tbnl:session-value 'user) nil)
  (bounce))

(defun auth-pane ()
  (let ((session (tbnl:session-value 'user)))
      (if session
	  (with-html-output-to-string (s)
	    ((:div :class "auth")
	     (fmt "Logged in as <b>~A</b>." (username session))
		 ((:a :href (make-url "/logout.html")) (str "Log out."))
		 (when (root session)
		   (htm ((:a :href (make-url "/userdetails.html"))
			 " Edit users.")
			((:a :href (make-url "/room.html"))
			 " View lisp image details.")
			((:a :href (make-url "/sessions.html"))
			 " View active sessions.")))
	     ((:a :href (add-get-params (make-url "/userdetails.html")
					"username" (username session)))
		   " Change user details.")
	     ((:a :href (make-url "/master-object-list.html"))
		  " View master object list.")))
	  "")))

(publish-page "/room.html"
	      (lambda ()
		(secure-page
		 (lambda () (and (tbnl:session-value 'user)
			    (root (tbnl:session-value 'user))))
		 (lambda ()
		   (make-page "Lisp statistics"
			      :header (htmlo (:h1 "Lisp details"))
			      :content (htmlo
					 (:pre
					(str (let* ((s (make-string-output-stream))
						    (*standard-output* s))
					       (room t)
					       (get-output-stream-string s))))))))))

(publish-page "/login.html"
	      (lambda ()
		(make-page "login page"
			   :header (htmlo (:h1 "Login page"))
			   :content (htmlo
				      (:p ((:form :action "/auth.html" :method "post")
					   (:table
					    (:tr
					     (:td (str "Login:"))
					     (:td (:input :type "textfield" :name "username")))
					    (:tr
					     (:td (str " Password:"))
					     (:td (:input :type "password" :name "password")))
					    (:tr (:td (:input :type "submit" :value "Go"))))))))))

(defun change-privs (user)
  (tbnl:log-message :error "Changing user privileges for ~A" (username user))
  (setf (privs user) (intern-from-list
		      (mapcar #'car
			      (remove-if-not (lambda (o)
					       (string-equal (cdr o) "on"))
					     (tbnl:post-parameters)))))
  (dump-user-info)
  (tbnl:redirect (add-get-params "/userdetails.html" "username" (username user))))

(defun priv-form (user)
  (tbnl:log-message :error "Printing user details form")
  (with-html-output-to-string (s)
    ((:form :action "/userdetails.html" :method :post)
     (:input :type "hidden" :name "changeprivs" :value "yes")
     (:input :type "hidden" :name "username" :value (username user))
     (dolist (l *linked-classes*)
       (if (has-priv user l)
	   (htm (:input :type "checkbox" :name (symbol-name l) :checked "on"))
	   (htm (:input :type "checkbox" :name (symbol-name l))))
       (str (make-presentable (symbol-name l)))
       (htm (:br)))
     (:input :type "submit" :value "Change privileges"))))

(defun password-form (user)
  (with-html-output-to-string (s)
    ((:form :action "/userdetails.html" :method :post)
     (:table
      (:tr (:td "Old password")
	   (:td (:input :type "password" :name "oldpass")))
      (:tr (:td "New password")
	   (:td (:input :type "password" :name "newpass1")))
      (:tr (:td "Repeat new password")
	   (:td (:input :type "password" :name "newpass2"))))
     (:input :type "hidden" :name "username" :value (username user))
     (:input :type "submit" :value "Change password"))))

(defun change-password (user oldpass newpass1 newpass2)
  (with-html-output-to-string (s)
    (if (and (or (equalp (md5:md5sum-sequence oldpass) (password user))
		 (root (tbnl:session-value 'user)))
	     (string-equal newpass1 newpass2))
	(progn
	  (setf (password user) (md5:md5sum-sequence  newpass1))
	  (dump-user-info)
	  (htm (:i "Password successfully changed")))
	(htm (:b "Either the old password was not correct, or the new passwords did not match."
		 ((:a :href (add-get-params
			     (make-url "/userdetails.html")
			     "username" (username user)))
		  "Please try again."))))))
	  
(defun new-user-form ()
  (with-html-output-to-string (s)
    ((:form :action "/userdetails.html" :method :post)
     (:table
      (:tr (:td "Username")
	   (:td (:input :type "textfield" :name "newusername")))
      (:tr (:td "Password")
	   (:td (:input :type "password" :name "pass1")))
      (:tr (:td "Repeat password")
	   (:td (:input :type "password" :name "pass2")))
      (dolist (c *linked-classes*)
	(htm (:tr (:td (str (make-presentable (symbol-name c))))
		  (:td (:input :type "checkbox" :name (symbol-name c))))))
      (:input :type "hidden" :name "addnewuser" :value "yes")
      (:input :type "submit" :value "Add new user")))))

(defun add-new-user ()
  (with-html-output-to-string (s)
    (with-tbnl-parameters (newusername pass1 pass2)
      (let ((privs (mapcar #'car
			   (remove-if-not (lambda (o)
					    (string-equal (cdr o) "on"))
					  (tbnl:post-parameters)))))
	(cond ((not newusername) (str "Missing username."))
	      ((find newusername *users* :key #'username :test #'string-equal)
	       (str "Username already exists!"))
	      ((not pass1) (str "You must enter a password."))
	      ((not (string-equal pass1 pass2))
	       (str "Passwords do not match"))
	      (t (progn
		   (new-user newusername pass1
			     (intern-from-list privs) nil)
		   (dump-user-info)
		   (fmt "User ~A added." newusername))))))))

