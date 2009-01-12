(in-package #:mess)

(defmethod display-short ((obj concept))
  (with-slots (name) obj
    (with-html-output-to-string (s)
      ((:a :href (make-view-url obj))
       (str name)))))

(defmethod display-nano ((obj concept))
  (format nil "~A" (name obj)))

