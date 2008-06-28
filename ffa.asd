(defpackage #:ffa-asd
  (:use :cl :asdf))

(in-package :ffa-asd)

(defsystem ffa
  :description "Foreign friendly arrays"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package")
	       (:file "pointer" :depends-on ("package")))
  :depends-on (:cffi :cl-utilities :metabang-bind :iterate :array-operations))

