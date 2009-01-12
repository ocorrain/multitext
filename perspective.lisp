(in-package #:mess)

(defmethod display ((obj perspective))
  (with-html-output-to-string (s)
    (:h1 (str (name obj)))
    (str (author obj))
    (str  (display-data obj))))

(defmethod display-short ((obj perspective))
  (with-html-output-to-string (s)
    ((:a :href (make-view-url obj))
     (str (name obj)))))

(defmethod display-nano ((obj perspective))
  (name obj))

(defmethod display-heading ((obj perspective))
  (with-slots (name author)
      obj
    (with-html-output-to-string (s)
      (:h1 (str name))
      (when author
	(htm (:p (:i (str author))))))))
