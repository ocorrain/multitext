(in-package #:mess)

(defmethod display-short ((obj case-study))
  (with-slots (name) obj
    (with-html-output-to-string (s)
      ((:a :href (make-view-url obj))
	   (str name)))))

(defmethod display-nano ((obj case-study))
  (name obj))
