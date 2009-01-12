(in-package #:mess)



(defmethod find-topic ((obj subtopic))
  (car (find-objects-of-type 'topic (mapcar #'find-instance (xrefs obj)))))

(defmethod find-topic ((obj linked-class))
  (or (car (find-objects-of-type 'topic (mapcar #'find-instance (xrefs obj))))
      (when-bind (subtopic (find-if (lambda (obj)
				      (member (find-class 'subtopic)
					      (class-precedence-list
					       (class-of obj))))
				    (mapcar #'find-instance (xrefs obj))))
	(find-topic subtopic))))

(defmethod display-data ((obj chronology))
  (htmlo
    (:h1 (str (name obj)))
    (when (get-priv 'chronology)
      (flet ((not-empty? (parm)
	       (and (tbnl:parameter parm) (not (zerop (length parm))))))
	(when (and (not-empty? "year") (not-empty?  "description")) ; minimum needed for a valid event
	  (when-bind (o (->make 'event (increment-object-counter)))
	    (update-object o (tbnl:post-parameters))
	    (link o obj)))) 
      (htm ((:form :action (make-view-url obj) :method "post")
	    (:table (:tr (dolist (it '("day" "month" "year" "to-day" "to-month" "to-year"))
			   (htm (:td (str (string-capitalize it)))
				(:td (:input :type "textfield" :size 4 :name it))))))
	    (:input :type "textfield" :size 60 :name "description")
	    (:input :type "submit" :value "Add event"))))
    (if-bind (events (safe-sort (apply #'append (remove nil (mapcar (lambda (c)
							  (find-linked-objects-of-type c 'event))
							(cons obj (find-linked-objects-of-type obj 'chronology)))))
				#'object-sort))
	     (str (print-events-as-html (gather-events events)))
	     (str "This chronology is empty!"))))

(defmethod display-short ((obj chronology))
  (htmlo ((:a :href (make-view-url obj)) (str (name obj)))))

(defmethod display ((obj event))
  (htmlo (:table (:tr (:td (str (year obj)))
		      (:td (:i (fmt "~A ~A" (or (day obj) "") (or (month obj) ""))))
		      (:td (str (description obj)))))))

(defmethod display-as-row ((obj event))
  (htmlo (:tr (:td (str (year obj)))
	      (:td (:i (fmt "~A ~A" (or (day obj) "") (or (month obj) ""))))
		      (:td (str (description obj))))))

(defmethod update-object :after ((obj event) alist)
  (declare (ignore alist))
  (log-msg 4 "Updating event ~A" (oid obj))
  (dolist (slot '(day month year to-day to-month to-year))
    (when (stringp (slot-value obj slot))
      (if-bind (numeric (ignore-errors
			  (parse-integer (slot-value obj slot)
					 :junk-allowed t)))
	       (setf (slot-value obj slot) numeric)
	       (setf (slot-value obj slot) nil)))))

(defmethod object-sort ((obj1 event) (obj2 event))
  (flet ((f (a b)
	   (cond ((null a) t)
		 ((null b) nil)
		 ((and (numberp a) (numberp b))
		  (< a b))
		 (t nil))))
    (or (f (year obj1) (year obj2))
	(f (month obj1) (month obj2))
	(f (day obj1) (day obj2)))))

(defun gather-events (list-of-events)
  (when list-of-events
    (flet ((s (list)
	     (safe-sort list (lambda (a b) (cond ((null a) t)
					    ((null b) nil)
					    ((and (numberp a) (numberp b))
					     (< a b)))) :key #'car)))
      (mapcar (lambda (l)
		(cons (car l) (mapcar (lambda (o) (cons (car o) (s (gather-list (cdr o) #'day))))
				      (s (gather-list (cdr l) #'month))))) 
	      (s (gather-list list-of-events #'year))))))


(defun gather-list (list &optional (func #'identity))
  (flet ((f (elt)
	   (equal (funcall func (car list))
		  (funcall func elt))))
    (if (null list)
	nil
	(cons (cons (funcall func (car list)) (remove-if-not #'f list)) 
	      (gather-list (remove-if #'f list) func)))))

(defun print-events (gathered-list)
  (dolist (y gathered-list)
    (format t "~&~A~%" (car y))
    (dolist (m (cdr y))
      (when (car m)
	(format t "~&    ~A~%" (find-month (car m)))) 
      (dolist (d (cdr m))
	(when (car d)
	  (format t "~&        ~A~%" (car d)))
	(dolist (e (cdr d))
	  (format t "~&            ~A~%" (description e)))))))

(defun print-events-as-html (gathered-list)
  (htmlo
    ((:table :cellpadding 2)
     (:colgroup (:col :width "7%") (:col :width "7%") (:col :width "90%"))
     (dolist (y gathered-list)
       (let ((year nil))
	 (dolist (m (cdr y))
	   (let ((month nil))
	     (dolist (d (cdr m))
	       (let ((day nil))
		 (dolist (e (cdr d))
		   (htm ((:tr :valign "top")
			 (:th (unless (or (null (year e)) (member (year e) year))
				(str (year e))
				(push (year e) year)))
			 ((:td :align "right") (fmt "<i>~A~A</i>"
				   (if (or (null (month e)) (member (month e) month))
				       ""
				       (progn (push (month e) month)
					      (format nil "~A " (find-month (month e)))))
				   (if (or (null (day e)) (member (day e) day))
				       ""
				       (progn (push (day e) day)
					      (day e)))))
			 (:td (str (description e)))
			 (when (get-priv 'event)
			   (htm (:td (:small
			       ((:a :href (add-get-params (make-url "/edit.html") "oid" (oid e))) (str "[edit]"))
			       (:br)
			       ((:a :href (add-get-params (make-url "/delete.html") "oid" (oid e))) (str "[delete]"))))))))))))))))))


(let ((months (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec")))
  (defun find-month (number)
    (when (and (> number 0) (<= number 12))
      (nth (- number 1) months))))

(defparameter *months* '())