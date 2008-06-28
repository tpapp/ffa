(defpackage :ffa
  (:use :common-lisp :cl-utilities :bind :iterate :array-operations)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export 

   ;; pointer

   with-pointer-to-array with-pointers-to-arrays))
