(in-package #:mess)


;; (defun search-box (&optional (url "/search.html") (label "Search ") hidden)
;;   (with-html-output-to-string (s)
;; 			      (:b "Search UCC electronic texts")
;;     (str "<!-- Google CSE Search Box Begins -->
;; <form id=\"searchbox_002927904911724867201:0lt5l2ofp3i\" action=\"http://google.com/cse\">
;;   <input type=\"hidden\" name=\"cx\" value=\"002927904911724867201:0lt5l2ofp3i\" />
;;   <input name=\"q\" type=\"text\" size=\"25\" />
;;   <input type=\"submit\" name=\"sa\" value=\"Search\" />
;;   <input type=\"hidden\" name=\"cof\" value=\"FORID:0\" />
;; </form>
;; <script type=\"text/javascript\" src=\"http://google.com/coop/cse/brand?form=searchbox_002927904911724867201%3A0lt5l2ofp3i\"></script>
;; <!-- Google CSE Search Box Ends -->"

;; )))

(defun search-box (&optional (url "/search.html") (label "Search ") hidden)
  (with-html-output-to-string (s)
			      (:b "Search Multitext and <a href=\"http://www.ucc.ie/celt/\">CELT</a>")
    (str "<!-- Google CSE Search Box Begins -->
<form id=\"searchbox_002927904911724867201:0lt5l2ofp3i\" action=\"http://google.com/cse\">
  <input type=\"hidden\" name=\"cx\" value=\"002927904911724867201:0lt5l2ofp3i\" />
  <input name=\"q\" type=\"text\" size=\"25\" />
  <input type=\"submit\" name=\"sa\" value=\"Search\" />
  <input type=\"hidden\" name=\"cof\" value=\"FORID:0\" />
</form>
<script type=\"text/javascript\" src=\"http://google.com/coop/cse/brand?form=searchbox_002927904911724867201%3A0lt5l2ofp3i\"></script>
<!-- Google CSE Search Box Ends -->"

))) 

(defvar *hidden-fields* '(oid))
(defparameter *css-dir* (merge-pathnames "css/" *mess-program-site*))

(setf *excluded-fields* (nconc *excluded-fields* (list 'filename 'file-content-type)))


;; (push (hunchentoot:create-folder-dispatcher-and-handler  "/images/" *image-directory*)
;;       hunchentoot:*dispatch-table*)

(push (hunchentoot:create-folder-dispatcher-and-handler "/styles/" *css-dir*)
      hunchentoot:*dispatch-table*)

(setf (html-mode) :sgml)

(defun create-cached-folder-dispatcher-and-handler
    (uri-prefix base-path &key content-type cache-control expires)
  "Based on hunchentoot's create-folder-dispatcher-and-handler, this sets Cache-Control and/or Expires headers before handling the file."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (error "~S must be string ending with a slash." uri-prefix))
  (when (or (pathname-name base-path)
            (pathname-type base-path))
    (error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (let* ((script-name (hunchentoot:url-decode (hunchentoot:script-name)))
                  (script-path (hunchentoot::enough-url (cl-ppcre:regex-replace-all "\\\\" script-name "/")
                                           uri-prefix))
                  (script-path-directory (pathname-directory script-path)))
             (unless (or (stringp script-path-directory)
                         (null script-path-directory)
                         (and (listp script-path-directory)
                              (eq (first script-path-directory) :relative)
                              (loop for component in (rest script-path-directory)
                                    always (stringp component))))
               (setf (hunchentoot:return-code) +http-forbidden+)
               (throw 'handler-done nil))
	     (when cache-control
	       (setf (hunchentoot:header-out "Cache-Control") cache-control))
	     (when expires
	       (setf (hunchentoot:header-out "Expires") expires))
             (hunchentoot:handle-static-file (merge-pathnames script-path base-path) content-type))))
    (hunchentoot:create-prefix-dispatcher uri-prefix #'handler)))

(push (create-cached-folder-dispatcher-and-handler  "/images/" *image-directory*
								:cache-control "max-age=1209600")
      hunchentoot:*dispatch-table*)


(defun make-page (title &key (header "") (nav (generic-navbar)) (content "") (sidebar "")
		  (image-path "/images/multitext_logo.png"))
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html (:head (:title (fmt "Multitext - ~A" title))
		  (:link :rel "stylesheet" :type "text/css" :href (make-url "/styles/multitext.css"))
		  (str +js-menu-snippit+))
	   (:body  ((:div :id "header")
		    ((:table :width "100%")
		     (:tr (:td ((:a :href "http://www.ucc.ie")
				(:img :border 0 :src (make-url "/images/ucclogo.gif") :alt "UCC logo")))
						  
			  (:td ((:p :align "center")
				((:a :href "http://multitext.ucc.ie")
				 (:img :border 0 :src (make-url "/images/multitextlogo.gif") :alt "Multitext logo"))
				(:br)
				(str header)) 
			       ((:td :align "right" :valign "center") (str (search-box)))))))
		   
		   ((:div :id "container")
		    (when (and nav (not (zerop (length nav)))) (htm ((:div :id "menu") (str nav))  (:br)))
		    ((:div :id "navBeta")
		     (str (auth-pane))
		     (str sidebar))
		    ((:div :class "contentwide")
		    
		     (str content)))))))


(defmethod obj->autolink ((obj linked-class) oid url)
  (with-form (url)
    (fmt (slots->form obj))
    (:p (:input :type "checkbox" :name "autolink" :value oid))))

(defun autolinker (obj)
  (with-html-output-to-string (s)
    (:input :type "checkbox" :name "link-this" :value (oid obj))
    (fmt "Link to ~A" (display-short obj))))

(defmethod object->html-form ((obj linked-class) &key (autolink nil))
  (let* ((class (class-of obj))
	 (classname (symbol-name (class-name class)))
	 (actionurl (format nil "/edit.html")))
    (with-form (actionurl)
      (:h1 (str (string-capitalize classname)))
      (fmt (slots->form obj))
      (if autolink
	  (str (autolinker (find-instance autolink)))))))


(defmethod object->html-form ((obj upload) &key (autolink nil))
  (let* ((class (class-of obj))
	 (class-name (class-name class)))
    (with-form ("/edit.html")
      (:h1 (fmt "~A" class-name))
      (fmt "~A" (slots->form obj))
      (:p (:b (str "File to upload: "))
	  (:input :type "file" :name "filename"))
      (if autolink
	  (str (autolinker (find-instance autolink)))))))

(defun get-slots (obj)
  (mapcar #'slot-definition-name (class-slots (class-of obj))))

(defmethod slots->form ((obj linked-class))
  (with-html-output-to-string (s)
    (:table
     (dolist (slot (get-slots obj))
      (when (not (member slot *excluded-fields*))
	(if (member slot *hidden-fields*)
	    (htm (:input :type "hidden"
			 :name (string-downcase (symbol-name slot)) 
			 :value (slot-value obj slot)))
	    (htm (:tr (:td (:b (str (make-presentable (symbol-name slot)))))
		      (let* ((raw (slot-value obj slot))
			     (val (if raw (format nil "~A" raw) ""))
			     (length (cond ((stringp val) (length val))
					   ((numberp val) (length (format nil "~A" val)))
					   (t 20))))
			(htm (:td (if (> length 60)
				      (htm (:td ((:textarea :name (string-downcase (symbol-name slot))
							    :rows 5 :cols 60)
						 (str val))))
				      (htm (:td (:input :type "textfield"
							:name (string-downcase (symbol-name slot))
							:value (tbnl:escape-for-html val) :size length)))))))))))))))

(defun slots->html (obj slots)
  (with-html-output-to-string (s)
    (dolist (slot slots)
    (when (not (member slot *excluded-fields*))
      (if (member slot *hidden-fields*)
	  (htm (:input :type "hidden"
		       :name (string-downcase (symbol-name slot)) 
		       :value (slot-value obj slot)))
	  (htm (:tr (:td (:b (str (make-presentable (symbol-name slot)))))
		    (let ((val (slot-value obj slot)))
		      (if val
			  (let ((length (length-of val)))
			    (if (> length 60)
				(htm (:td ((:textarea :name (string-downcase (symbol-name slot))
						      :rows 5 :cols 60)
					   (str val))))
				(htm (:td (:input :type "textfield"
						  :name (string-downcase (symbol-name slot))
						  :value (tbnl:escape-for-html val) :size (length-of val))))))
			  (htm (:td (:input :type "textfield"
					    :name (string-downcase (symbol-name slot))
					    :size "60"))))))))))))

(defmethod display-nano ((obj linked-class))
  "")

(defmacro htmlo (&body body)
  (let ((s (gensym)))
    `(with-html-output-to-string (,s)
       ,@body)))

(defun render-standard-page (title render-function
			     &key (stream *standard-output*) environment session additional-headers)
  (declare (ignore environment session stream))
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:head
     (:title (str title))
     (:link :href (make-url "/multitext.css") :rel "stylesheet" :type "text/css")
     (when additional-headers (str (funcall  additional-headers))))
    (:body
     (str (funcall render-function)))))

(defun class-name-from-url (url)
  (let ((pos (1+ (position "/" url :test #'string-equal :from-end t))))
    (subseq url pos)))

(defun reap-objects ()
  (dolist (o *objects*)
    (if (null-object o)
	(delete-object o))))

(let ((nulls '(oid xrefs)))
  (defun null-object (obj)
    "An object is null if all of its fields, save OID and xrefs, are blank."
    (let* ((slots (remove-if #'(lambda (o)
				 (member o nulls))
			     (mapcar #'slot-definition-name (class-slots (class-of obj)))))
	   (vslots (remove-if #'(lambda (slot)
				  (null (slot-value obj slot))) slots)))
      (not vslots))))

(defun tabular-object-linker (oid &rest types)
  (labels ((print-tabular-objects (objs)
	     (let* ((class-names (remove-duplicates (mapcar (lambda (o)
							      (class-name (class-of o))) objs))))
	       class-names))
	   (interesting (obj)
	     (if types
		 (intersection types
			       (mapcar #'class-name
				       (class-precedence-list (class-of obj))))
		 t))
	   (make-object-list (type objects value)
	     (let ((objs (safe-sort (remove-if-not (lambda (obj)
						     (equal (class-name (class-of obj)) type))
						   objects)
				    #'object-sort)))
	       (with-html-output-to-string (s)
		 (dolist (o objs)
		   (htm (:input :type "checkbox" :name (oid o) :value value)
			(str (display-short o))
			(:br))))))
	   (tabulate (objects value)
	     (with-html-output-to-string (s)
	       (:table
		(let ((class-names (print-tabular-objects objects)))
		  (htm (:tr
			(dolist (c class-names)
			  (htm (:td (:b (str (make-presentable (symbol-name c))))))))
		       ((:tr :valign "top") 
			(dolist (c class-names)
			  (htm (:td (str (make-object-list c objects value))))))))))))
    (let* ((xrefs (remove-if-not #'interesting (find-linked-by-oid oid)))
	   (unxrefs (set-difference (remove-if-not #'interesting *objects*) xrefs)))
      (with-html-output-to-string (s)
	(when xrefs
	  (htm ((:form :action "/linkobj.html" :method "post")
		(:input :type "submit" :value "Delete cross-reference")
		(:input :type "hidden" :name "oid" :value oid)
		(str (tabulate xrefs "off")))))
	(:hr)
	(when unxrefs
	      (htm ((:form :action "/linkobj.html" :method "post")
		    (:input :type "submit" :value "Cross-reference")
		    (:input :type "hidden" :name "oid" :value oid)
		    (str (tabulate unxrefs "on")))))))))


(defun object-linker (oid expanded &rest types)
  (if expanded
      (flet ((interesting (obj)
	       (if types
		   (intersection
		    types
		    (mapcar #'class-name
			    (class-precedence-list (class-of obj))))
		   t))
	     (bundle-as-types (x)
	       (safe-sort x #'string< :key (lambda (y)
					     (symbol-name (class-name (class-of y)))))))
	
	(let* ((xrefs (remove-if-not #'interesting (find-linked-by-oid oid)))
	       (unxrefs (set-difference (remove-if-not #'interesting *objects*) xrefs)))
	  (with-html-output-to-string (s)
	    ((:form :action "/viewobj.html" :method "post")
	     (:input :type "hidden" :name "oid" :value oid)
	     (:input :type "submit" :value "Collapse"))
	    (when xrefs
	      (htm (:h2 "Cross-references")
		   ((:form :action "/linkobj.html" :method "post")
		    (:input :type "submit" :value "Delete cross-reference")
		    (:input :type "hidden" :name "oid" :value oid)
		    (:table
		     (dolist (v xrefs)
		       (htm (:tr (:td (:input :type "checkbox" :name (oid v) :value "off"))
				 (:td (fmt "<b>~A:</b> ~A" (class-name (class-of v)) (display-short v))))))))))
	    (:hr)
	    (when unxrefs
	      (htm ((:form :action "/linkobj.html" :method "post")
		    (:input :type "submit" :value "Cross-reference")
		    (:input :type "hidden" :name "oid" :value oid)
		    (:table
		     (dolist (v unxrefs)
		       (when (not (or (member (oid v) xrefs)
				      (= (oid v) oid)))
			 (htm (:tr (:td (:input :type "checkbox" :name (oid v) :value "on"))
				   (:td (fmt "<b>~A:</b> ~A" (class-name (class-of v)) (display-short v))))))))))))))
      (with-html-output-to-string (s)
	((:form :action "/viewobj.html" :method "post")
	 (:input :type "hidden" :name "oid" :value oid)
	 (:input :type "hidden" :name "link" :value "yes")
	 (:input :type "submit" :value "Cross-reference...")))))





(defun find-class-from-string (string)
  (let ((symbol (read-from-string (string-upcase string))))
    (find-class symbol)))

(defun canonicalize-parameters (params)
  (tbnl:log-message :error "Cleaning ~A" params)
  (remove-if #'null params :key #'cdr))

(defun assoc-val (item alist)
  (cdr (assoc item alist :test #'string-equal)))

(defmethod display-short ((obj linked-class))
  (display obj))

(defmethod display ((obj linked-class))
  (let* ((class (class-of obj))
	 (class-name (class-name class))
	 (slots (mapcar #'slot-definition-name (class-slots class))))
    (with-html-output-to-string (s)
      (:table
       (:tr (:td (:h3 (str class-name))))
       (dolist (slot slots)
	 (when (not (or (member slot *excluded-fields*)
			(member slot *hidden-fields*)))
	   (let ((val (slot-value obj slot)))
	     (when val
	       (htm (:tr
		 (:td (:b (str (make-presentable (symbol-name slot)))))
		 (:td (fmt "~S" (text->html val)))))))))))))

(defmethod display-nano ((obj contributor))
  (full-name obj))

(defmethod display ((obj contributor))
  (with-html-output-to-string (s)
    (:h1 (str (display-nano obj)))
    (when-bind (bio (short-biography obj))
      (htm (:p (str bio))))
    (:p (fmt "Articles: ~{~A~^; ~}"
	     (mapcar #'display-link (remove-if (lambda (c) (member (class-name (class-of c))
								   '(event picture topic contributor)))
					       (find-linked-objects obj))))))) 

;; (remove-if (lambda (c) (member c '(event picture topic contributor))) *linked-classes*)
;; ))))


(defun get-oid-param ()
  (get-tbnl-number "oid"))




(defun linked-class-selector (&key
			      (add nil)
			      (label "Create new")
			      (destination "/createnew.html")
			      (autolink nil)
			      (additional-parms nil)
			      (method "get")
			      (quiet nil))
  (flet ((get-editable-classes ()
	   (when-bind (user (tbnl:session-value 'user))
		      (remove-if-not #'(lambda (c)
					 (has-priv user c))  *linked-classes*))))
    (if-bind (rel (get-editable-classes))
	     (let ((sorted-list (sort (mapcar #'stringify (if add
							      (append rel (list add))
							      rel)) #'string<)))
	       (with-html-output-to-string (s)
		 (if (not quiet)
		     (htm ((:form :action destination :method method)
			   (:input :type "submit" :value label)
			   (when additional-parms
			     (dolist (ap additional-parms)
			       (htm (:input :type "hidden"
					    :name (format nil "~A" (first ap))
					    :value (format nil "~A" (second ap))))))
			   (when autolink
			     (htm (:input :type "hidden" :name "autolink" :value autolink)))
			   ((:select :name "type")
			    (dolist (elt sorted-list)
			      (htm ((:option :value elt)
				    (str (make-presentable elt))))))))
		     (htm (fmt "~{~A~^, ~}"
			       (mapcar (lambda (elt)
					 (with-html-output-to-string (s)
					   ((:a :href (make-url (add-get-params
								 destination
								 "type" elt "autolink" autolink)))
					    (str (string-downcase (make-presentable elt)))))) sorted-list))))))
	     "")))


(defun class-selector (&key (label "View type"))
  (let ((sorted-list (sort (mapcar #'stringify (append *linked-classes* (list 'all)))
			   #'string<)))
    (with-html-output-to-string (s)
      (:input :type "submit" :value label)
      ((:select :name "type")
       (dolist (elt sorted-list)
	 (htm ((:option :value elt)
	       (str (make-presentable elt)))))))))


(defun make-presentable (str)
  (string-capitalize (cl-ppcre:regex-replace-all "-" str " ")))



(defmethod display-short ((obj field))
  (with-html-output-to-string (s)
    (with-slots (name start-date end-date) obj
      (htm (fmt "~A, ~A&ndash;~A." name start-date end-date)))))

(defmethod display-short ((obj area))
  (with-html-output-to-string (s)
    (with-slots (name start-date end-date) obj
      (htm ((:a :href (make-view-url obj))
	    (:b (fmt "~A, ~A&ndash;~A" name start-date end-date)))))))

(defmethod make-view-url ((obj linked-class)  &key (host t))
  (if (oid->name (oid obj))
      (make-url (list "/d/~A" (oid->name (oid obj))) :host host)
      (add-get-params (make-url "/display.html" :host host) "oid" (oid obj))))

(defmethod display-link ((obj linked-class))
  (with-html-output-to-string (s)
    (str (display-short obj))))

(defmethod display-link ((obj personality))
  (with-html-output-to-string (s)
    ((:a :href (make-view-url obj)) (str (display-nano obj)))))

(defmethod make-view-url ((obj contributor) &key (host t))
  (add-get-params (make-url "/display.html" :host host) "oid" (oid obj)))

(defmethod display-link ((obj contributor))
  (with-html-output-to-string (s)
    ((:a :href (make-view-url obj)) (str (initials obj)))))

(defun add-button (oid type)
  (let ((name (symbol-name type)))
    (with-html-output-to-string (s)
      ((:form :action "/createnew.html" :method "post")
       (:input :type "hidden" :name "type" :value (format nil "~A" name))
       (:input :type "hidden" :name "autolink" :value oid)
       (:input :type "submit" :value (format nil "Add new ~A"
					     (string-downcase (make-presentable name))))))))

(defun remake-all-headings ()
  (dolist (obj (remove-if-not
	      (lambda (o)
		(member 'blob-data
			(mapcar #'class-name 
				(class-precedence-list
				 (class-of o)))))
	      *objects*))
    (format t "rescanning ~A~%" (display-nano obj))
    (setf (headings obj) (rewrite-headings (oid obj)))))





