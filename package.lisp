(in-package #:cl-user)

(defpackage #:mess
  (:use
   #:cl
   #:cl-who
   #:dobdob
   #:toc-utils
   #:web-utils
   #+cmu #:pcl
   #+openmcl #:ccl
))
