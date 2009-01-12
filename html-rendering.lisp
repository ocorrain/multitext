(in-package #:mess)

(defun secured (priv render-func)
  (lambda ()
    (secure-page priv render-func)))

(defun secure-page (priv render-func)
  "Secures a page. Priv is an argument with no
   functions -- if it evaluates to true, then the
   page is rendered using render-func. Otherwise,
   a forbidden message is returned."
  (if (funcall priv)
      (progn (log-msg 1 (format nil "Granted ~A to ~A" priv (tbnl:session-value 'user)))
	     (funcall render-func))
      (progn
	(log-msg 1 (format nil "~A forbidden to ~A" priv (tbnl:session-value 'user)))
	(setf (tbnl:return-code) tbnl:+http-forbidden+)
	(make-page "Forbidden" :header "Forbidden"
		   :content (htmlo (:h1 "Forbidden")
				   (:p "You don't have permission to access this!")
				   (:p ((:form :action "/auth.html" :method "post")
					(:table
					 (:tr
					  (:td (str "Login:"))
					  (:td (:input :type "textfield" :name "username")))
					 (:tr
					  (:td (str " Password:"))
					  (:td (:input :type "password" :name "password")))
					 (:tr (:td (:input :type "submit" :value "Go")))))))))))

(defun get-url-name (obj)
  (cdr (assoc (oid obj) *url-names*)))

(defun get-url-oid (name)
  (cdr (assoc name *url-names* :test #'string-equal :key #'cdr)))

(defun oid->name (oid)
  (cdr (assoc oid *url-names*)))

(defun name->oid (name)
  (car (rassoc name *url-names* :test #'string-equal)))

(defun safe-sort (sequence predicate &key key)
  (sort (copy-seq sequence) predicate :key key))

(defun text->html (text)
  "Takes a string and tries to render it as nice HTML"
  (when (stringp text)
    (cl-ppcre:regex-replace-all  "\\n+" text "<p>")))

(defun index-page ()
  (flet ((random-xrefed-object (type)
	   (random-elt (remove nil (find-objects-of-type type)
			       :key #'xrefs))))
    (make-page "Welcome to the Cork Multitext Project"
	       ;:nav ""
	       :sidebar (htmlo
			  (when (cl-ppcre:scan "MSIE" (tbnl:user-agent))
			    (htm (:p "This site is best viewed with Firefox. The Cork Multitext Project site uses PNG images and cascading stylesheets, neither of which is well supported under Internet Explorer.  We suggest the free browser, Firefox, as an alternative. Click on the button below to download Firefox for your operating system."
				     (:br)
				     (str "<a href=\"http://www.spreadfirefox.com/?q=affiliates&id=0&t=70\"><img border=\"0\" alt=\"Get Firefox!\" title=\"Get Firefox!\" src=\"http://sfx-images.mozilla.org/affiliates/Buttons/88x31/get.gif\"/></a>"))))
			  (fmt "~{<center>~A</center>~^<br><center>~~ o ~~ </center><br>~}"
			       (mapcar (lambda (type)
					 (if (eq type 'picture)
					     (display-thumb (random-xrefed-object type))
					     (display-short (random-xrefed-object type))))
				       '(picture personality concept case-study))))
	       :content (htmlo
			  (str
			   (list-class-instances (htmlo
						   (:h1 "Books")) 'topic "<ul>~{<li>~A</li>~}</ul>"))
			  (str (site-summary))))))

(defun site-summary ()
  "<h2>Project Description</h2> 

    <p>The MultiText Project in History is an innovative educational
project, brought to you by the History Department, University College
Cork. It is the largest and most ambitious project undertaken by any
university to provide resources for students of Modern Irish History
at all levels: University students, the general reader, and
second-level students.  The project aims to publish a minimum of 12
books, each dealing with a separate period of Irish history. Each book
contains accounts of key personalities, concepts, and detailed
elucidations of some case studies in the period. </p>


    <h2>Contact details</h2>

    <p>Contact Tom&aacute;s O'Riordan <a
       href=\"mailto:ta.oriordan@ucc.ie\">ta.oriordan at
       ucc dot ie</a> (Project Manager) with any comments or enquiries
       about the content of the site, and Tiarn&aacute;n &Oacute;
       Corr&aacute;in <a href=\"mailto:ocorrain@gmail.com\">ocorrain
       at gmail dot com</a> (Technical) with comments or enquiries
       about technical matters.</p>

    <h2>Funding</h2>
    <p>We gratefully acknowledge the generous support of the President's Fund, University 
College Cork, and of Mr. Joseph Carey.</p>

    <h2>Resources</h2>

    <p>It provides all the necessary resources to enable teachers and students to engage fully with 
the new Leaving Certificate syllabus essays; document-based case studies; key 
concepts; and biographies of key personalities. Extensive bibliographies and illustrations 
are also provided.</p>

    <h2>Time Scale</h2>

    <p>Work on late Modern Irish History (1815-2000) is nearing completion and materials on 
all six topics will appear on this site over the next three to six months. Books on Early 
Modern Irish History (1494-1814) are in progress and sample texts can be viewed by 
clicking the top menu. </p>

    <h2>Editorial</h2>
    <p>All contributions are credited to their authors/editors. Different authors bring varied 
interpretations and understandings to the work. No effort has been made to edit out these 
differences of opinion and interpretation. Historians differ for many genuine reasons, and 
students must learn that. </p>")
			   

(setf tbnl:*default-handler* #'index-page)

;(setf tbnl:*show-lisp-errors-p* t)

;; The meat of the HTML rendering goes here
(defun list-of (title list &key (sort t))
  (with-html-output-to-string (s)
    (if list
	(htm (:h2 (str title))
	     (:ol (dolist (elt (if sort
				   (safe-sort list #'object-sort)
				   list))
		    (htm (:li (str (display-link elt)))))))
	;; Move along, nothing to see here
	"")))

(defun bounce ()
  "Redirects to the first available object in the history
   list, or to the index page, if none exist"
  (labels ((redir (stack)
	     (if stack
		 (let ((obj (find-instance (car stack))))
		   (if obj
		       (progn
			 (setf (tbnl:session-value 'redirect-stack) (cdr stack))
			 (tbnl:redirect (make-view-url obj :host nil)))
		       (redir (cdr stack))))
		 (tbnl:redirect "/view.html"))))
    (redir (tbnl:session-value 'redirect-stack))))

(defun bounce-url (&optional (element 0))
  "Gives the URL of the next object to be bounced on"
  (let ((stack (tbnl:session-value 'redirect-stack)))
    (if (> element (1- (length stack)))
	(bounce-url (1- element))
	(with-html-output-to-string (s)
	  (str (display-short (find-instance (elt stack element))))))))


(defun lists-of-linked-objects (&key (objects *objects*) (classes *linked-classes*))
  (let (output)
    (dolist (l classes)
      (let ((objects (find-objects-of-type l objects)))
	(when objects
	  (push (list-of (make-presentable (symbol-name l)) objects) output))))
    output))

(defmethod render :before ((obj linked-class))
  ; clear up any null objects
  (reap-objects))

(defmethod render ((obj linked-class))
  (make-page (display-nano obj)
	     :content (display obj)))

(defmethod table-of-contents ((obj standard-display-mixin))
  (when-bind (headings (headings obj))
    (let ((midpoint (ceiling (/ (length headings) 2))))
      (flet ((print-section (list)
	       (htmlo ((:td :valign "top")
		       (:ul (dolist (elt list)
			      (htm (:li ((:a :href (format nil "#~A" (cadr elt)))
					     (str (car elt)))))))))))
	(htmlo (:h3 "Contents")
	       (:small (:table
			(:tr (str (print-section (subseq headings 0 midpoint)))
			     (str (print-section (subseq headings (1+ midpoint))))))) )))))

(defun class-page (type title)
  (lambda ()
    (make-page title
	     :header (htmlo ((:h1 :class "header") (str title)))
	     :content (htmlo
			(:ul
			 (dolist (o (safe-sort
				     (find-objects-of-type type)
				     #'object-sort))
			   (htm (:li (str (display-short o))))))))))

(dolist (type '(("/personality" personality "Key personalities")
		("/concept" concept "Key concepts")
		("/case-study" case-study "Case studies")
		("/perspective" perspective "Perspectives")))
  (publish-page (first type) (class-page (second type) (third type))))

(defmethod render ((obj contributor))
  (make-page (display-nano obj)
	       :header (format nil "Contributor&mdash;~A" (full-name obj))
	       :nav (make-navigation obj)
	       :sidebar (htmlo (str (display-image obj))
			       (when (get-priv (class-name (class-of obj)))
				 (str (edit-bar obj))))
	       :content (display obj)))

(defmethod render :around ((obj standard-display-mixin))
  (let* ((xrefs (lists-of-linked-objects
		 :classes (remove-if (lambda (c) (member c '(event picture topic contributor))) *linked-classes*)
		 :objects (find-linked-objects obj))))

    (make-page (display-nano obj)

	       :header (if-bind (topic (find-topic obj))
				(htmlo ((:h1 :class "header") (str (display-link topic)))) 
				(htmlo ((:h1 :class "header") "Cork Multitext Project")))

	       :nav (make-navigation obj)

	       :sidebar (htmlo (str (display-image obj))
			       (when (get-priv (class-name (class-of obj)))
				 (str (edit-bar obj)))
			       (htm ((:div :id "xrefs")
				     (dolist (x xrefs)
				 (htm (str x))))))
	       

	       :content (htmlo (:h1 (str (display-nano obj)))
			       (when-bind (contributors (find-linked-objects-of-type obj 'contributor))
				 (htm (:p (fmt "Contributors: ~{~A~^, ~}."
					       (mapcar #'display-link contributors)))))
			       
			       (when (slot-exists-p obj 'headings)
				 (when-bind (headings (headings obj))
				   (htm ((:div :id "tableofcontents")
					 (str (table-of-contents obj)))))) 
			       (let ((dd (display-data obj)))
				 (if dd
				     (htm ((:div :id "maintext")
					   (str dd)))))))))

(defun generic-navbar ()
  (htmlo ((:ul :id "nav")
	  (:li ((:a :href (make-url "/")) "Home"))
	  (dolist (menu '(("Key concepts" concept) ("Personalities" personality)
			  ("Case studies" case-study ) ("Perspectives" perspective)))
	    (htm (:li ((:a :href (make-url (format nil "/~A" (string-downcase (second menu)))))
			   (str (first menu)))))))))

(defun make-nav (topic &optional (type "topic"))
  (flet ((navlist (class obj)
	   (htmlo (:ul (dolist (thing (safe-sort
				       (find-linked-objects-of-type obj class) #'object-sort))
			 (htm (:li (str (display-link thing)))))))))
    (htmlo ((:ul :id "nav")
	     (:li ((:a :href "/") "Home"))
	     (dolist (menu '(("Key concepts" concept) ("Personalities" personality)
			     ("Case studies" case-study ) ("Perspectives" perspective)))
	       (if (eq type (second menu))
		   (htm (:li ((:a :href (make-url (format nil "/~A" (string-downcase (second menu)))))
						 (str (first menu))) 
			 (str (navlist (second menu) topic))))
		   (htm (:li ((:a :href (make-url (format nil "/~A" (string-downcase (second menu)))))
						 (str (first menu)))
			 (str (navlist (second menu) topic))))))    ))))


(defconstant +js-menu-snippit+
"<script type=\"text/javascript\"><!--//--><![CDATA[//><!--
startList = function() {
	if (document.all&&document.getElementById) {
		navRoot = document.getElementById(\"nav\");
		for (i=0; i<navRoot.childNodes.length; i++) {
			node = navRoot.childNodes[i];
			if (node.nodeName==\"LI\") {
				node.onmouseover=function() {
					this.className+=\" over\";
				}
				node.onmouseout=function() {
					this.className=this.className.replace(\" over\", \"\");
				}
			}
		}
	}
}
window.onload=startList;

//--><!]]></script>")


(defmethod find-topic ((obj subtopic))
  (car (find-objects-of-type 'topic (mapcar #'find-instance (xrefs obj)))))

(defmethod make-navigation ((obj linked-class))
  (when-bind (topic (find-topic obj))
    (make-nav (find-topic obj) (type-of obj))))

(defmethod make-navigation ((obj subtopic))
  (make-nav (find-topic obj) (type-of obj)))

(defmethod make-navigation ((obj topic))
  (make-nav obj))


(defmethod render :before ((obj linked-class))
  (when (not tbnl:*session*)
    (tbnl:start-session)
    (setf (tbnl:session-value 'redirect-stack) nil))
  (setf (tbnl:session-value 'redirect-stack)
	(cons (oid obj) (remove (oid obj) (tbnl:session-value 'redirect-stack)))))


(defun last-links (&optional (number 5))
  (let* ((stack (tbnl:session-value 'redirect-stack))
	 (recent (subseq stack 0 (min number (length stack)))))
    (if recent
	(with-html-output-to-string (s)
	  (:h3 "Recently visited")
	  (:ul
	   (dolist (r recent)
	     (when (find-instance r)
	       (htm (:li (str (display-short (find-instance r)))))))))
	"")))



(defun random-elt (seq)
  (when seq
    (elt seq (random (length seq)))))

(defmethod navigation-bar ((obj subtopic))
  (with-html-output-to-string (s)
    ((:div :class "navbar")
     ((:a :href (make-url "/"))
      "HOME")
     " >> "
     (when-bind (topic (car (find-linked-objects-of-type obj 'topic)))
       (htm ((:a :href (make-view-url topic))
	     (str (display-nano topic))))))))


(defmethod printable ((obj subtopic))
  (with-html-output-to-string (s)
    (:h1 (str (display-heading obj)))
    (str (display-data obj))))

(defmethod edit-bar :around ((obj linked-class))
  (with-html-output-to-string (s)
    ((:div :class "auth")
     (str (make-button "Edit"
		       (add-get-params "/edit.html" "oid"
				       (oid obj)))) (:br)
     (str (make-button "Cross reference"
		       (add-get-params "/cross-refs.html" "oid"
				       (oid obj)))) (:br)
      (if (next-method-p) (str (call-next-method))) (:hr)
     (:b "Create a new ")
     (str (linked-class-selector :label "Create new object"
				 :autolink (oid obj) :quiet t))
     (:hr)
     (str (make-button "Delete"
		       (add-get-params "/delete.html" "oid"
				       (oid obj)))))))

(defmethod edit-bar ((obj linked-class))
  "")

(defmethod edit-bar ((obj blob-data))
  (make-button "View raw text"
	       (add-get-params "/raw-text.html" "oid"
			       (oid obj))))
 

(define-page view-printable "/printable/"
  (let ((oid (read-from-string (last1 (cl-ppcre:split "/" (tbnl:script-name))))))
    (when oid
      (with-html
	((:div :class "printable")
	 (str (printable (find-instance oid)))
	 (:hr)
	 (str "Published by the Cork Multitext Project (multitext.ucc.ie) ."))))))

(defmethod rawtext ((obj subtopic))
  (with-html-output-to-string (s)
    (str (display-data obj))))

(define-page view-raw-text "/raw-text.html"
  (let ((oid (get-oid-param)))
    (when (and oid (get-priv (class-name (class-of (find-instance oid)))))
      (with-html
	(str (rawtext (find-instance oid)))))))

(defmethod printable ((obj linked-class))
  (display obj))

(define-page render-page "/display.html"
  (when-bind (oid (tbnl:parameter "oid"))
    (let ((inst (find-instance (parse-integer oid))))
      (if inst
	  (render inst)
	  (setf (tbnl:return-code) tbnl:+HTTP-NOT-FOUND+)))))

(define-page render-from-name "/d/"
  (when-bind (oid (name->oid
		   (last1
		    (cl-ppcre:split "/"
				    (tbnl:script-name)))))
    (when-bind (inst (find-instance oid))
      (render inst))))

(defun last1 (list)
  (car (last list)))

(defun write-xrefs (objs)
  (let ((type (class-from-obj (car objs))))
    (with-html-output-to-string (s)
      (:h3 (str (make-presentable (symbol-name type))))
      (let ((output (mapcar #'(lambda (x)
				(display-short x))
			    (sort (copy-list objs) #'object-sort))))
	(htm (str (format nil "~{~A~^; ~}." output)))
	output))))

(defun display-xrefs (objects)
  "Takes a list of cross-references and sorts
   them according to type, the prints a nice table"
    (let ((classes
	   (sort (copy-list
		  (remove-duplicates (mapcar #'class-from-obj objects)))
		  #'string< :key #'symbol-name)))
      (with-html-output-to-string (st)
	(dolist (class classes)
	  (htm (str (write-xrefs
		     (remove-if-not
		      #'(lambda (obj)
			  (equal (class-from-obj obj) class)) objects))))))))

(defun class-from-obj (obj)
  (class-name (class-of obj)))

(defmethod display-heading ((obj linked-class))
  (display-short obj))

(defun list-class-instances (title class &optional (format-string "~{~A~^; ~}."))
  (let ((objects (safe-sort (find-objects-of-type class) #'object-sort)))
    (with-html-output-to-string (s)
      (str title)
      (:p (fmt format-string (mapcar #'display-link objects))))))

(publish-page "/about.html"
	      (lambda ()
		(make-page "About the Multitext Project"
			   :sidebar (htmlo
				      (:h2 "People")
				      (:p (:b "Chairman")
					  (:br)
					  "Mr Joseph Carey P.C., D.D.Sc., D.B.Adm., M.I.A.V.I."
					  (:br)
					  "Managing Director,"
					  (:br)
					  "REMAX Cork & County Auctioneers.")
				      (:p (:b "Project Director")
					  (:br)
					  "Donnchadh &Oacute; Corr&aacute;in" (:br)
					  "@" ((:a :href "mailto:d.ocorrain@ucc.ie")
					   "d.ocorrain at ucc.ie") (:br)
					   "Professor of Medieval History" (:br)
					   "University College Cork.")
				      (:p (:b "Project Manager")
					  (:br)
					  "Tom&aacute;s O&rsquo;Riordan MPhil HDipEd" (:br)
					  "@" ((:a :href "mailto:TA.ORiordan@ucc.ie")
					   "ta.oriordan at ucc.ie"))
				      (:p (:b "Technical Architect")
					  (:br)
					  "Tiarn&aacute;n &Oacute; Corr&aacute;in" (:br)
					  "@" ((:a :href "mailto:ocorrain@yahoo.com")
						  "ocorrain at yahoo.com")))
			   :content (htmlo
				      (:p "The project aims to publish six books, each dealing with a
            separate period of Irish history. Each book contains accounts
            of key personalities, concepts, and detailed elucidations of
            some case studies in the period. A sample of these is listed
            below.")
				      (:h2 "Funding")
				      (:p "We gratefully acknowledge the generous support of the President&rsquo;s Fund, University College Cork, and of Mr. Joseph Carey.")

				      (:h2 "Project Description")
				      (:p "It is the largest and most ambitious project
undertaken by any university to provide resources for students of
Modern Irish History at all levels&mdash;University students, the
general reader, and especially second-level students.")

				      (:h2 "Resources")
				      (:p "It provides all the resources to enable teachers and
students to engage fully with the new Leaving Certificate
syllabus&mdash; essays and documents on each case study; key
concepts; and biographies of key personalities. Extensive
bibliographies and illustrations are provided.")

				      (:h2 "Time Scale")
				      (:p "Work on late Modern Irish History (1815&ndash;2000) is nearing completion and
materials on all six topics will appear on this site over the next three to
months. The topics are: ")
				      (:ol
				       (:li "Ireland and the Union, 1815&ndash;1870")
				       (:li "Movements for Reform and Political Change, 1870&ndash;1914")
				       (:li "The Pursuit of Sovereignty and the Impact of Partition, 1912&ndash;1949")
				       (:li "Government, Economy &amp; Society in the Republic of Ireland, 1949&ndash;2000")
				       (:li "Politics and Society in Northern Ireland, 1949&ndash;2000")
				       (:li "The Irish Diaspora, 1840&ndash;1966."))

				      (:p "Work on Early Modern Irish History is in progress and sample texts
will appear on this site very shortly.")

				      (:h2 "Editorial")
				      (:p "All contributions are credited to their
authors/editors. Different authors bring varied interpretations
and understandings to the work. No effort has been made to edit
out these differences of opinion and interpretation. Historians
differ for many genuine reasons, and students must learn that.")))))

