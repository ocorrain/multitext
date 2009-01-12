(in-package #:mess)

(defun swap-up-with-same-type (item list)
  (let ((pos (position item list)))
    (when (and pos (/= pos 0 ))
      (when-bind (next (find-if (lambda (elt)
				  (eq (type-of elt)
				      (type-of item)))
				(subseq list 0 pos) :from-end t))
	(rotatef (elt list pos) (elt list (position next list)))))
    list))

(defun swap-up-xref (obj item)
  (setf (xrefs obj)
	(mapcar #'oid
		(swap-up-with-same-type item
					(mapcar #'find-instance (xrefs obj))))))

(defun swap-down-xref (obj item)
  (let ((objs (find-linked-objects-of-type obj (type-of item))))
    (when objs
      (when (< (position item objs) (length objs))
	(swap-up-xref obj (elt objs (1+ (position item objs))))))))

(defun swap-top-xref (obj item)
  (setf (xrefs obj) (cons (oid item)
			  (remove (oid item) (xrefs obj)))))

(defun swap-end-xref (obj item)
  (setf (xrefs obj) (append (remove (oid item) (xrefs obj))
			    (list (oid item)))))

(defmethod render :before ((obj gallery))
  (when (get-priv 'picture)
    (when-bind (swapup (tbnl:parameter "swapup"))
      (let ((xref (read-from-string swapup)))
	(when (numberp xref)
	  (swap-up-xref obj (find-instance xref)))))
    (when-bind (swapdown (tbnl:parameter "swapdown"))
      (let ((xref (read-from-string swapdown)))
	(when (numberp xref)
	  (swap-down-xref obj (find-instance xref)))))
    (when-bind (swaptop (tbnl:parameter "swaptop"))
      (let ((xref (read-from-string swaptop)))
	(when (numberp xref)
	  (swap-top-xref obj (find-instance xref)))))
    (when-bind (swapend (tbnl:parameter "swapend"))
      (let ((xref (read-from-string "swapend")))
	(when (numberp xref)
	  (swap-end-xref obj (find-instance xref)))))))

(defmethod render ((obj gallery))
  (let ((slideshow (tbnl:parameter "slideshow")))
    (if slideshow
	(let ((cleaned (read-from-string slideshow)))
	  (when (numberp cleaned)
	    (view-slide-show obj cleaned)))
	(make-page (display-nano obj)
		   :nav (make-navigation obj)
		   :header (htmlo (:h1 (str (display-nano obj))))
		   :content (htmlo
			      (:p "Related entries: "
				  (fmt "~{~A~^; ~}." (mapcar #'display-link
							     (remove-if (lambda (o) (equal (type-of o) 'picture))
									(find-linked-objects obj)))))
			      (:p ((:a :href (add-get-params (make-view-url obj) "slideshow" 0)
				       :class "display")
				   "View slideshow"))
			      (str (gallery (find-linked-objects-of-type obj 'picture))))))))


(defmethod find-topic ((obj gallery))
  (when-bind (subtopic (find-if (lambda (obj)
				  (member (find-class 'subtopic)
					  (class-precedence-list
					   (class-of obj))))
				(mapcar #'find-instance (xrefs obj))))
    (find-topic subtopic)))

(defmethod display ((obj gallery))
  (let ((slideshow (tbnl:parameter "slideshow")))
    (if slideshow
	(let ((cleaned (read-from-string slideshow)))
	  (when (numberp cleaned)
	    (view-slide-show obj cleaned)))
	(with-html-output-to-string (s)
	  ((:div :id "navBetawide")
	   ;; (str (search-box))
	   ((:div :class "auth")
	    (str (authentication-pane (oid obj))))
	   (dolist (x (lists-of-linked-objects
		       :classes (remove 'picture *linked-classes*)
		       :objects (find-linked-objects obj)))
	     (htm ((:div :class "contentbox")
		   (str x))))
	   ((:div :class "contentbox")
	    (str (last-links))))
	  (when (get-priv (class-name (class-of obj)))
	    (htm ((:div :class "contentwide")
		  (str (edit-bar obj)))))
	  ((:div :class "contentwide")
	   (:h1 (str (display-nano obj)))
	   (:p ((:a :href (add-get-params (make-view-url obj) "slideshow" 0) :class "display")
		"View slideshow"))
	   )))))

(defun view-slide-show (obj index)
  (when-bind (images (find-linked-objects-of-type obj 'picture))
    (if (and (>= index 0)
	     (< index (length images)))
	(let ((i (elt images index)))
	  (with-slots (short-caption long-caption source filename) i
	    (make-page (display-nano i) ;:nav ""
		       :image-path nil
		       :header (htmlo ((:h1 :class "header") (fmt "Gallery: ~A" (display-link obj)))) 
		       :sidebar (htmlo (if (> index 0)
					   (htm ((:a :href (add-get-params
							    (make-url (tbnl:script-name))
							    "slideshow" (1- index)))
						 "Previous"
					;; (:img :class "slideshow"
					;; 					      :src (make-url "/images/larrow.png")
					;; 					      :alt "Previous")
						 ))
					   (str "Start"))
		       
				       (str " | ")

				       (if (< index (1- (length images)))
					   (htm ((:a :href (add-get-params
							    (make-url (tbnl:script-name))
							    "slideshow" (1+ index)))
						 ;; 					(:img :class "slideshow"
						 ;; 					      :src (make-url "/images/rarrow.png")
						 ;; 					      :alt "Next")
						 "Next"
						 ))
					   (str "End"))

				       

				       (when short-caption
					 (htm (:h2 (str short-caption))))
				       (:center (fmt "(~A of ~A)"  (+ 1 index) (length images)))
				       (when source
					 (htm ((:p :class "caption")
					       (:b "Source: ")
					       (:i (str source)))))
				       (when long-caption
					 (htm ((:p :class "caption") (str long-caption)))))
		       :content (htmlo  ((:p :align "middle")
					 (:img :src (concatenate 'string "/images/" filename)))
;; 				  ((:table :width "100%" :cellspacing 10)
;; 				   (:tr ((:td :valign "middle")
;; 					 (if (> index 0)
;; 					     (htm )
;; 					     ""))
;; 					((:td :valign "top")
;; 					 (if filename
;; 					     (htm ((:p :align "middle")
;; 						   (:img :src (concatenate 'string "/images/" filename))))
;; 					     (str "No image found")))
;; 					((:td :valign "middle")
;; 					 (if (< index (1- (length images)))
;; 					     (htm )
;; 					     (str "End")))))
				  )))))))


	    



