(defpackage #:ffa-asd
  (:use :cl :asdf))

(in-package :ffa-asd)

(defsystem ffa
  :description "Foreign friendly arrays"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package")
	       (:file "types" :depends-on ("package"))
	       (:file "ffa" :depends-on ("types"))
	       (:file "operations" :depends-on ("ffa"))
	       (:file "pointer" :depends-on ("ffa")))
  :depends-on (:cffi :cl-utilities :metabang-bind :iterate))

