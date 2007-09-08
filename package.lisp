(defpackage :ffa
  (:use :common-lisp :cl-utilities :bind :iterate)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export 

   ;; ffa

   make-ffa displace-array find-original-array
   find-or-displace-to-flat-array

   ;; operations

   array-reduce array-max array-min array-sum array-product
   array-count array-range outer-product

   ;; pointer

   with-pointer-to-array))
