; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(defpackage #:mess-system
  (:use #:cl
	#:asdf))

(in-package #:mess-system)

(defsystem mess
  :components ((:file "package")
	       (:file "site" :depends-on ("package"))
	       (:file "mess" :depends-on ("site"))
	       (:file "html" :depends-on ("mess"))
	       (:file "pages" :depends-on ("html" "html-rendering"))
	       (:file "casestudy" :depends-on ("html"))
	       (:file "celt-text" :depends-on ("html"))
	       (:file "concept" :depends-on ("html"))
	       (:file "picture" :depends-on ("html"))
	       (:file "gallery" :depends-on ("picture"))
	       (:file "personality" :depends-on ("html"))
	       (:file "perspective" :depends-on ("html"))
	       (:file "internet-reference" :depends-on ("html"))
	       (:file "text" :depends-on ("html"))
	       (:file "topic" :depends-on ("html"))
	       (:file "users" :depends-on ("html"))
	       (:file "html-rendering" :depends-on ("html"))
	       (:file "cross-references" :depends-on ("html-rendering"))
	       (:file "news" :depends-on ("html-rendering"))
	       (:file "treat-file" :depends-on ("mess"))
	       (:file "index" :depends-on ("html-rendering"))
	       (:file "time" :depends-on ("html-rendering")))
  
  :depends-on (:hunchentoot :kmrcl :cl-who :dobdob :toc-utils :web-utils))






