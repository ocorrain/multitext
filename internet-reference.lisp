(in-package #:mess)

(defmethod display-short ((obj internet-reference))
  (with-html-output-to-string (s)
    (with-slots (oid author title year publisher editor) obj
      (when author
	(htm (fmt "~A, " author)))
      (when title
	(htm (:i ((:a :href (url obj)) (fmt "~A " title)))))
      (when year
	(htm (fmt "(~A)." year)))
      (when editor
	(htm (fmt " ~A (ed)." editor))))))