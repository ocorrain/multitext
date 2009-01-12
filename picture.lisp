(in-package #:mess)

(defmethod image-tag ((picture image))
  (with-html-output-to-string (s)
    (:img :src (format nil "/images/~A" (filename picture)))))


(defmethod display-image ((obj linked-class))
  (let ((images (find-linked-objects-of-type obj 'picture)))
    (if images
	(with-html-output-to-string (s)
	  (htm ((:div :class "thumbnail")
		(let ((random (random-elt images)))
		  (str (display-thumb random))
		  (if (> (length images) 1)
		      (htm (:br)
			   ((:a :href (make-url
				       (format nil "/viewgallery/~A" (oid obj))))
			    (fmt "View gallery (~A)" (length images)))
			   (str " | ")
			   ((:a :href (make-url
				       (format nil "/viewgallery/~A?slideshow=0"
					       (oid obj))))
			    (str "Slide show"))))))))
	"")))

(defmethod display-nano ((obj picture))
  (format nil "~A" (short-caption obj)))

(defmethod display-nano ((obj gallery))
  (or (name obj) ""))

(defmethod navigation-bar :around ((obj linked-class))
  (with-html-output-to-string (s)
    ((:div :class "navigation")
     (str (call-next-method))
     ((:div :class "userinfo")
      (str (authentication-pane (oid obj)))))))

(defmethod navigation-bar ((obj picture))
  (with-html-output-to-string (s)
    ((:div :class "navbar")
     ((:a :href (make-url "/"))
      "HOME")
     " >> "
     (str (bounce-url 1)))))

(defmethod render ((obj picture))
  (let* ((xrefs (lists-of-linked-objects
		 :classes (remove 'picture *linked-classes*)
		 :objects (find-linked-objects obj))))
    (with-slots (short-caption source long-caption filename) obj
      (make-page (display-nano obj)
		 :image-path "/images/multitext_smalllogo.png"
		 :nav ""
;		 :header short-caption
		 :sidebar (htmlo
			    (when (get-priv 'picture)
			      (str (edit-bar obj)))
			    ((:div :class "contentbox")
  			     (when short-caption
 			       (htm (:h2 (str short-caption))))
			     (when long-caption
			       (htm (:p (str long-caption))))
			     (when source
			       (htm (:br)
				    (:b "Source: ")
				    (:i (str source)))))
			    ((:div :id "xrefs") (dolist (x xrefs)
			      (htm ((:div :class "contentbox")
				    (str x))))) )
		 :content (when filename
			    (htmlo ((:p :align "center")
				    (:img :src (concatenate 'string "/images/" filename)))))))))

(defmethod edit-bar ((obj picture))
  "")


(publish-page
 "/viewgallery"
 (lambda ()
   (let* ((oid (read-from-string (last1 (cl-ppcre:split "/" (tbnl:script-name)))))
	  (obj (find-instance oid))
	  (images (find-linked-objects-of-type obj 'picture))
	  (slideshow (tbnl:parameter "slideshow")))
     (if slideshow
	 (let ((cleaned (read-from-string slideshow)))
	   (when (numberp cleaned)
	     (view-slide-show obj cleaned)))
	 (make-page (display-nano obj)
		    :nav (make-navigation obj)
		    :header (htmlo ((:h1 :class "header") (fmt "Gallery: ~A" (display-short obj))))
		    :content (htmlo 
			       (:p ((:a :href (add-get-params (make-url (tbnl:script-name)) "slideshow" 0) :class "display")
				    "View slideshow"))
			       ((:p :align "center")
				(if (get-priv 'picture)
				    (htm (str (gallery images 
						       :image-render-function (lambda (img)
										(let ((url (make-url (tbnl:script-name))))
										  (with-html-output-to-string (s)
										    ((:table :width "100%")
										     (:tr (:td
											   ((:a :href (add-get-params url "swaptop" (oid img)))
											    "top"))
											  (:td ((:a :href (add-get-params url "swapup" (oid img)))
												"up"))
											  (:td ((:a :href (add-get-params url
															  "swapdown" (oid img)))
												"down"))
											  (:td ((:a :href (add-get-params url
															  "swapend" (oid img)))
												"bottom"))))
										    (str (display-thumb img))))))))
				    (htm (str (gallery images)))))))

;; 	 (with-std-page ((display-nano obj))
;; 	   ((:div :class "contentwide")
;; 	    (:h2 (fmt "Gallery: ~A" (display-short obj)))
;; 	    (:p "Thumbnail images of the gallery are displayed below. To view the full-sized
;;                 image and a long caption, click on the short caption below each image. To view
;;                 a slideshow of the images in the gallery, click on \"View slideshow\" below.")
;; 	    (:p ((:a :href (add-get-params (make-url (tbnl:script-name)) "slideshow" 0) :class "display")
;; 		 "View slideshow"))
;; 	    ((:p :align "center")
;; 	     (if (get-priv 'picture)
;; 		 (htm (str (gallery images 
;; 				    :image-render-function (lambda (img)
;; 							     (let ((url (make-url (tbnl:script-name))))
;; 							       (with-html-output-to-string (s)
;; 								 ((:table :width "100%")
;; 								  (:tr (:td
;; 									((:a :href (add-get-params url "swaptop" (oid img)))
;; 									 "top"))
;; 								       (:td ((:a :href (add-get-params url "swapup" (oid img)))
;; 									     "up"))
;; 								       (:td ((:a :href (add-get-params url
;; 												       "swapdown" (oid img)))
;; 									     "down"))
;; 								       (:td ((:a :href (add-get-params url
;; 												       "swapend" (oid img)))
;; 									     "bottom"))))
;; 								 (str (display-thumb img))))))))
;; 		 (htm (str (gallery images)))))))
	 ))))



(defun partition-list (psize list &optional collected)
  (cond ((null list) collected)
	((>= psize (length list)) (append collected (list list)))
	(t (partition-list psize
			   (subseq list psize)
			   (append collected (list (subseq list 0 psize)))))))

(defun gallery (images &key (image-render-function
			     (lambda (img)
			       (htmlo
				 (str (display-thumb img))
				 ((:div :class "thumbnail")
				  (str (teaser 150 (long-caption img)))))))
		(vertical 3))
  (with-html-output-to-string (s)
    ((:table :cellpadding 10)
     (dolist (image-row (partition-list vertical images))
       (htm (:tr
	     (dolist (image image-row)
	       (htm (:td (str (funcall image-render-function image)))))))))))

(defmethod display-thumb ((obj picture))
  (with-html-output-to-string (s)
    ((:div :class "thumbnail")
     (:img :src (format nil "/images/thumbnails/~A" (filename obj))
	   :alt (short-caption obj))
     (when-bind (sc (short-caption obj))
       (htm (:br)
	    (:i (str (display-short obj))))))))



(defmethod display ((obj picture))
  (with-slots (short-caption source long-caption filename) obj
    (let ((image-url (concatenate 'string "/images/" filename)))
      (with-html-output-to-string (s)
	(when short-caption
	  (htm (:h3 (str short-caption))))
	(when filename
	  (htm (:img :src image-url)))
	(when source
	  (htm (:br) (:center (:b "Source: ")
			      (:i (str source)))))
	(when long-caption
	  (htm (:p (str long-caption))))))))

(defmethod display-short ((obj picture))
  (with-slots (short-caption filename) obj
    (with-html-output-to-string (s)
      (when (name obj)
	(htm ((:a :href (make-view-url obj))
	      (str (name obj)))))
      (when (short-caption obj)
	(htm ((:a :href (make-view-url obj))
	      (str short-caption)))))))

(defmethod display-short ((obj gallery))
  (with-html-output-to-string (s)
    ((:a :href (make-view-url obj))
     (str (name obj)))))

(defun make-thumbnail (input-filename output-filename dimension &optional (quality 100))
  (multiple-value-bind (string-out error-out exit-status)
      (kmrcl:command-output "~A -quality ~A -resize ~Ax~A ~A ~A"
			    *convert-program*
			    quality dimension dimension
			    input-filename
			    output-filename)
    (declare (ignore string-out))
    (if (/= exit-status 0)
	(log-msg 1 "make-thumbnail -- ImageMagick errval ~A, <<~A>>" exit-status error-out)
	(multiple-value-bind (string-out error-out exit-status)
	    (kmrcl:command-output "chmod og+r ~A" output-filename)
	  (declare (ignore string-out))
	  (when (/= exit-status 0)
	    (log-msg 1 "chmod errval ~A <<~A>>" exit-status error-out))))))

(defun remake-all-thumbs (&optional really-all?)
  (log-msg 1 "Refreshing images: ~A" (print-formatted-time (get-universal-time)))
  (flet ((munge (x y)
	   (concatenate 'string x y)))
    (dolist (p (find-objects-of-type 'picture))
      (log-msg 2 "remaking ~A: ~A" (oid p) (short-caption p))
      (let ((image-filename (filename p)))
	(when image-filename
	  (let ((image-path (concatenate 'string (blob-directory) image-filename)))
	    (when (probe-file image-path)
	      (unless (or (probe-file (munge *image-directory* image-filename)) really-all?)
		(make-thumbnail image-path (munge *image-directory* image-filename) 600))
	      (unless (or (probe-file (munge *thumbs-directory* image-filename)) really-all?)
		(make-thumbnail image-path (munge *thumbs-directory* image-filename) 300))))))))
  (log-msg 1 "Finished: ~A~%-----" (print-formatted-time (get-universal-time))))

(defmethod update-object :around ((obj image) alist)
  (log-msg 1 "updating image object: ~A" obj)
  (log-msg 3 "parameters: ~A" alist)
  (let ((image-info (cdr (assoc "filename" alist :test #'string-equal))))
    (when (and image-info (not (zerop (length image-info)))) 
      (let* ((temp-filename (first image-info))
	     (orig-filename (second image-info))
	     (content-type (third image-info))
	     (extension (first (last (cl-ppcre:split "\\." orig-filename))))
	     (image-filename (format nil "~A.~A" (oid obj) extension))
	     (image-path (format nil "~A~A" *image-directory* image-filename))
	     (thumbs-path (format nil "~A~A" *thumbs-directory* image-filename))
	     (blob-path (format nil "~A~A" (blob-directory) image-filename)))
	(when (probe-file temp-filename)
	  (kmrcl:copy-file temp-filename blob-path :overwrite t)
	  (make-thumbnail temp-filename image-path 600)
	  (make-thumbnail temp-filename thumbs-path 300)
	  (delete-file temp-filename))
	(setf (file-content-type obj) content-type)
	(setf (filename obj) image-filename)))
    (call-next-method obj (remove-if #'upload-specific alist))))

